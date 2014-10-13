%%%-------------------------------------------------------------------
%%% @author David Seida
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2014 8:20 PM
%%%-------------------------------------------------------------------
-module(broker_agent).
-author("David Seida").

%% API
-export([start/0, stop/0, status/0, new_rfp/1, loop/0]).

start() ->
  Pid = spawn(broker_agent, loop, []),
  register(broker_agent, Pid),
  ok.

%% Support testing from the shell
stop() ->
  broker_agent ! stop,
  ok.

%% Support testing from the shell
%% Use flush to see the response
status() ->
  broker_agent ! {status, self()},
  ok.

new_rfp(Problem) ->
  broker_agent ! {rfp, {self(),Problem}},
  receive
    {rfp_no, Rfp_no} -> Rfp_no
  end.


start_agents() ->
  Config1 = [agent1, [1], [2], [1]],
  Config2 = [agent2, [2], [3], [1]],
  Config3 = [agent3, [3], [4], [1]],
  Config4 = [agent4, [4], [5], [1]],
  Config5 = [agent5, [5], [6], [1]],
  Config6 = [agent6, [6], [7], [1]],
  Config7 = [agent7, [7], [8], [1]],
  Config8 = [agent8, [8], [9], [1]],
  Config9 = [agent9, [9], [10], [1]],
  Config10 = [agent10, [3],[5], [4]],
  Config11 = [agent11, [4], [7], [2]],
  Config12 = [agent12, [1], [12], [10]],
  Config13 = [agent13, [12], [11], [3]],
  spawn(standard_agent, start, [Config1]),
  spawn(standard_agent, start, [Config2]),
  spawn(standard_agent, start, [Config3]),
  spawn(standard_agent, start, [Config4]),
  spawn(standard_agent, start, [Config5]),
  spawn(standard_agent, start, [Config6]),
  spawn(standard_agent, start, [Config7]),
  spawn(standard_agent, start, [Config8]),
  spawn(standard_agent, start, [Config9]),
  spawn(standard_agent, start, [Config10]),
  spawn(standard_agent, start, [Config11]),
  spawn(standard_agent, start, [Config12]),
  spawn(standard_agent, start, [Config13]),
  [agent1, agent2, agent3, agent4, agent5, agent6, agent7, agent8, agent9, agent10, agent11, agent12, agent13].

loop() ->
  Agent_list = broker_agent:start_agents(),
  loop(Agent_list, 0, [], []).

loop(Agent_list, Last_rfp, Past_proposals, Current_rfps) ->
  receive
  %%  {Submittor, Input, Output} = Problem
    {rfp, {Pid, Problem}} ->
      {Submittor, Input, Output} = Problem,
      Rfp_list = review_problem(Agent_list, Last_rfp, Past_proposals, Problem) ++ Current_rfps,
      Pid ! {rfp_no, Last_rfp+1},
      loop(Agent_list, Last_rfp + 1, Past_proposals, Rfp_list);
    {bid, Proposal} ->
      Rfp_list = update_rfp(Current_rfps, Proposal),
      loop(Agent_list, Last_rfp, Past_proposals, Rfp_list);
    {no_bid, {Name, Rfp_no}} ->
      Rfp_list = no_bid(Name, Current_rfps, Rfp_no),
      loop(Agent_list, Last_rfp, Past_proposals, Rfp_list);
    {status, Pid} ->
      Pid ! {Agent_list, Last_rfp, Past_proposals, Current_rfps},
      loop(Agent_list, Last_rfp, Past_proposals, Current_rfps);
    stop -> ok
  end.

%% Trivial case - Inputs and Outputs match
%% Immediately send bid to broker
%% Return empty list
review_problem(Agent_list, Last_rfp, {Name, In, Out, Cost}, {Rfp_no, Submittor, In, Out}) ->
  broker_agent ! { bid, {Rfp_no, In, Out, [Name | Submittor], Cost}},
  [];
%% Case 1 - Inputs match but Outputs do not
%% If Agent is in the existing chain of bidders -> no bid
%% Otherwise, Send subproblem statement to broker
%% Return RFP status for addition to RFP list
review_problem(Agent_list, Last_rfp, {Name, In, Agent_Out, _}, {Rfp_no, Submittor, In, Out}) ->
  case lists:member(Name, Submittor) of
    true ->
      broker_agent ! {no_bid, {Name, Rfp_no}},
      [];
    false ->
      Problem = { [Name | Submittor], Agent_Out, Out},
      case broker_agent:new_rfp(Problem) of
        {ok, Assigned_rfp_no} ->
          [{Assigned_rfp_no, Rfp_no, Submittor, In, Out}];
        {error, _} ->
          broker_agent ! {no_bid, {Name, Rfp_no}},
          []
      end
  end;
%% Case 2 - Inputs don't match
%%  Send no bid to broker
review_problem(Agent_list, Rfp_no, {Name, _, _, _}, {Rfp_no, _, _, _}) ->
  broker_agent ! {no_bid, {Name, Rfp_no}},
  [].

%% When receiving no_bid from broker on requested subcontractors this
%% sends a no_bid on the prime rfp and removes the pending bid from
%% the RFP list.
%% Going through the full list twice -- inefficient, improve this later
%% [{Assigned_rfp_no, Prime_rfp_no, Submittor_list, In, Out}, ...] = Rfps
no_bid({Name, _, _, _}, Rfps, Rfp_no) ->
  [{_, Prime_rfp_no, _, _, _} | _] =
    lists:filter(fun({Assigned_rfp_no, _, _, _, _}) ->
      Assigned_rfp_no == Rfp_no end, Rfps),
  broker_agent ! {no_bid, {Name, Prime_rfp_no}},
  lists:filter(fun({Assigned_rfp_no, _, _, _, _}) ->
    Assigned_rfp_no /= Rfp_no end, Rfps).

%% Add mechanism to periodically delete rfps from list
%% Will require additional checks for late arriving proposals
%% (after deleted from the list)
update_rfp({Name, _, _, Cost}, Rfps, Proposal) ->
  {Rfp_no, _, _, Services, Prop_cost} = Proposal,
  [{_, Prime_rfp_no, _, In, Out} | _] =
    lists:filter(fun({Assigned_rfp_no, _, _, _, _}) ->
      Assigned_rfp_no == Rfp_no end, Rfps),
  broker_agent ! {bid, {Prime_rfp_no, In, Out,
    [Name | Services], Prop_cost + Cost}},
  Rfps.


