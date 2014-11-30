%%%-------------------------------------------------------------------
%%% @author davidseida
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Aug 2014 10:55 PM
%%%-------------------------------------------------------------------
-module(standard_agent).
-author("davidseida").

%% API
-export([start/1, stop/1, status/1, loop/2]).


%% start, stop, status are external access methods
%% all other methods are used internally and are not directly accessible
%%  {Name, Agent_In, Agent_Out, Cost} = Config
start(Config) ->
  {Name, _, _, _} = Config,
  Pid = spawn(standard_agent, loop, [Config, []]),
  register(Name, Pid),
  ok.

%% Support testing from the shell
stop(Agent_Name) ->
  Agent_Name ! stop,
  ok.

%% Support testing from the shell
%% Use flush to see the response
status(Agent_Name) ->
  Agent_Name ! {status, self()},
  ok.

loop(Config, Rfps) ->
  receive
    %%  {Reply_to, Input, Output, Prior_Nodes} = Cand_Rfp
    %%  [{Reply_to, In, Out, Prior_Nodes}, ...] = Rfps
    {rfp, Cand_Rfp} ->
      Rfp_list = review_rfp(Config, Cand_Rfp) ++ Rfps,
      loop(Config, Rfp_list);
    {bid, Proposal} ->
      Rfp_list = update_rfp(Config, Rfps, Proposal),
      loop(Config, Rfp_list);
    {survey, Pid, {Input, Output}} ->
      {Agent_Name, Agent_In, Agent_Out, _} = Config,
      send_msg(Pid, {survey_response, {Agent_Name, Input, Agent_In == Input, Output, Agent_Out == Output}}),
      loop(Config, Rfps);
    {no_bid, {_, Transform}} ->
      Rfp_list = no_bid_rfp(Config, Rfps, Transform),
      loop(Config, Rfp_list);
    {status, Pid} ->
      send_msg(Pid, {Config, Rfps}),
      %%Pid ! {Config, Rfps},
      loop(Config, Rfps);
    reset -> loop(Config, []);
    stop -> ok
  end.

%% Trivial case - Inputs and Outputs match
%% Immediately send bid to broker
%% Return empty list
review_rfp({Name, In, Out, Cost}, {_, In, Out, Prior_Nodes}) ->
  case lists:member(Out, Prior_Nodes) of
    true ->
      send_msg({no_bid, {Name, {In, Out, Prior_Nodes}}});
    false ->
      send_msg({bid, {In, Out, Prior_Nodes, [Name], Cost}})
  end,
  [];
%% Case 1 - Inputs match but Outputs do not
%% If Agent is in the existing chain of bidders -> no bid
%% Otherwise, Send subproblem statement to broker
%% Return RFP status for addition to RFP list
review_rfp({Name, In, Agent_Out, _}, {Reply_to, In, Out, Prior_Nodes}) ->
  case lists:member(Agent_Out, Prior_Nodes) of
    true ->
      send_msg({no_bid, {Name, {In, Out, Prior_Nodes}}}),
      [];
    false ->
      Problem = { [Name], Agent_Out, Out, [ In | Prior_Nodes]},
      case broker_agent:new_rfp(Problem) of
        {rfp_no, _} ->
          [{Reply_to, In, Out, Prior_Nodes}];
        {error, _} ->
          send_msg({no_bid, {Name, {In, Out, Prior_Nodes}}}),
          []
      end
  end;
%% Case 2 - Inputs don't match
%%  Send no bid to broker
review_rfp({Name, _, _, _}, {_, In, Out, Prior_Nodes}) ->
  send_msg({no_bid, {Name, {In, Out, Prior_Nodes}}}),
  [].

%% When receiving no_bid from broker on requested subcontractors this
%% sends a no_bid on the prime rfp and removes the pending bid from
%% the RFP list.
%% Going through the full list twice -- inefficient, improve this later
%% [{Assigned_rfp_no, Prime_rfp_no, Submittor_list, In, Out}, ...] = Rfps
no_bid_rfp({Name, In, _, _}, Rfps, {_, Output, [_ | Prior_Nodes]}) ->
  {Rfp_info, Rfp_list} = lists:partition(fun({_, In1, Out1, Prior_Nodes1}) ->
              {In1, Out1, Prior_Nodes1} == {In, Output, Prior_Nodes} end, Rfps),
  case Rfp_info of
    [] -> true;
    [_] -> send_msg({no_bid, {Name, {In, Output, Prior_Nodes}}})
  end,
  Rfp_list.

%% Add mechanism to periodically delete rfps from list
%% Will require additional checks for late arriving proposals
%% (after deleted from the list)
update_rfp({Name, Input, _, Cost}, Rfps, Proposal) ->
  { _In, Out,[_ | Prior_Nodes], Services, Prop_cost} = Proposal,
  Request_record =
    lists:filter(fun({ _, In1, Out1, Prior_Nodes1}) ->
      {In1, Out1, Prior_Nodes1} == {Input, Out, Prior_Nodes} end, Rfps),
  case Request_record of
    [] -> true;  %% do nothing
    _ -> [{_, Input, Out, _} | _] = Request_record,
         Circle_detect = lists:member(Name, Services),
         if
           Circle_detect -> do_nothing;
           true -> send_msg({bid, {Input, Out, Prior_Nodes, [Name | Services], cost:add(Prop_cost,Cost)}})
         end
  end,
  Rfps.

send_msg(Message) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", broker_agent}, Message},
  broker_agent ! Message.

send_msg(Pid, Message) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", Pid}, Message},
  Pid ! Message.

