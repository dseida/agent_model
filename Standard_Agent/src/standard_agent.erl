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
    %%  {Rfp_no, Submittor, Input, Output} = Cand_Rfp
    %%  {Assigned_Rfp_no, Prime_rfp_no,
    {rfp, Cand_Rfp} ->
      Rfp_list = review_rfp(Config, Cand_Rfp) ++ Rfps,
      loop(Config, Rfp_list);
    {bid, Proposal} ->
      Rfp_list = update_rfp(Config, Rfps, Proposal),
      loop(Config, Rfp_list);
    {no_bid, Rfp_no} ->
      Rfp_list = no_bid_rfp(Config, Rfps, Rfp_no),
      loop(Config, Rfp_list);
    {status, Pid} ->
      Pid ! {Config, Rfps},
      loop(Config, Rfps);
    stop -> ok
  end.

%% Trivial case - Inputs and Outputs match
%% Immediately send bid to broker
%% Return empty list
review_rfp({Name, In, Out, Cost}, {Rfp_no, _, In, Out}) ->
  broker_agent ! { bid, {Rfp_no, In, Out, [Name], Cost}},
  [];
%% Case 1 - Inputs match but Outputs do not
%% If Agent is in the existing chain of bidders -> no bid
%% Otherwise, Send subproblem statement to broker
%% Return RFP status for addition to RFP list
review_rfp({Name, In, Agent_Out, _}, {Rfp_no, Submittor, In, Out}) ->
  case lists:member(Name, Submittor) of
    true ->
      broker_agent ! {no_bid, {Name, Rfp_no}},
      [];
    false ->
      Problem = { [Name | Submittor], Agent_Out, Out},
      case broker_agent:new_rfp(Problem) of
        {rfp_no, Assigned_rfp_no} ->
          [{Assigned_rfp_no, Rfp_no, Submittor, In, Out}];
        {error, _} ->
          broker_agent ! {no_bid, {Name, Rfp_no}},
          []
      end
  end;
%% Case 2 - Inputs don't match
%%  Send no bid to broker
review_rfp({Name, _, _, _}, {Rfp_no, _, _, _}) ->
  broker_agent ! {no_bid, {Name, Rfp_no}},
  [].

%% When receiving no_bid from broker on requested subcontractors this
%% sends a no_bid on the prime rfp and removes the pending bid from
%% the RFP list.
%% Going through the full list twice -- inefficient, improve this later
%% [{Assigned_rfp_no, Prime_rfp_no, Submittor_list, In, Out}, ...] = Rfps
no_bid_rfp({Name, _, _, _}, Rfps, Rfp_no) ->
  {Rfp_info, Rfp_list} = lists:partition(fun({Assigned_rfp_no, _, _, _, _}) ->
                             Assigned_rfp_no == Rfp_no end, Rfps),
  case Rfp_info of
    [] -> true;
    [{_, Prime_rfp_no, _, _, _}] -> broker_agent ! {no_bid, {Name, Prime_rfp_no}}
  end,
  Rfp_list.

%% Add mechanism to periodically delete rfps from list
%% Will require additional checks for late arriving proposals
%% (after deleted from the list)
update_rfp({Name, _, _, Cost}, Rfps, Proposal) ->
  {Rfp_no, _, _, Services, Prop_cost} = Proposal,
  Request_record =
    lists:filter(fun({Assigned_rfp_no, _, _, _, _}) ->
      Assigned_rfp_no == Rfp_no end, Rfps),
  case Request_record of
    [] -> true;  %% do nothing
    _ -> [{_, Prime_rfp_no, _, In, Out} | _] = Request_record,
         broker_agent ! {bid, {Prime_rfp_no, In, Out,
               [Name | Services], cost:add(Prop_cost,Cost)}}
  end,
  Rfps.

