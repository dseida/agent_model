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
-export([start/0, stop/0, status/0, reset/0, new_rfp/1, loop/0, start_agents/0]).

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

reset() ->
  broker_agent ! reset.

new_rfp(Problem) ->
  %% add logic to send {error, "Invalid Problem"} to calling routine if Problem does not meet the form {Submittor, Input, Output}
  %% where Input and Output are lists within the tuple
  broker_agent ! {rfp, {self(),Problem}},
  receive
    {rfp_no, Rfp_no} -> {rfp_no, Rfp_no}
  end.


start_agents() ->
  Config1 = {agent1, [1], [2], [1]},
  Config2 = {agent2, [2], [3], [1]},
  Config3 = {agent3, [3], [4], [1]},
  Config4 = {agent4, [4], [5], [1]},
  Config5 = {agent5, [5], [6], [1]},
  Config6 = {agent6, [6], [7], [1]},
  Config7 = {agent7, [7], [8], [1]},
  Config8 = {agent8, [8], [9], [1]},
  Config9 = {agent9, [9], [10], [1]},
  Config10 = {agent10, [3],[5], [4]},
  Config11 = {agent11, [4], [7], [2]},
  Config12 = {agent12, [1], [12], [10]},
  Config13 = {agent13, [12], [11], [3]},
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
  Agent_list = start_agents(),
  loop(Agent_list, 0, [], []).

loop(Agent_list, Last_rfp, Past_proposals, Current_rfps) ->
  %%  Past_proposals = [{Rfp_no, Input, Output, Services, Cost}, ... ]
  %%  Rfp_list = [{Rfp_no, Submittor, Input, Output, Agent_bids, Agent_nobids}, ...]
  %%  Submittor = [ Agent_name]
  %%  Input = [ ...]
  %%  Output = [ ... ]
  %%  Agent_bids = [ {Agent_name, Bid_response}, ...]
  %%  Bid_response = { Services, Cost }
  %%  Services = [ AgentX, ...]
  %%  Cost = [ Cost_factor1, ...]
  %%  Agent_nobids = [ Agent_nameX, ...]
  receive
    %%  Problem = {Submittor, Input, Output}
    {rfp, {Pid, Problem}} ->
      %% 1)  Check to see if the problem has already been solved
      %%     If so, return the original RFP number and then mail the result to the requester
      %% 2)  Check to see if we are already working on the problem
      %%     If so, add the new submittor to the submittor list and return the rfp number to the new submittor
      %% 3)  Otherwise, add the problem to RFP_list, return the rfp number to the submittor, send the problem to the agents,
      %%     and increment the last rfp value
      {Submittor, Input, Output} = Problem,
      Previous_rfp = lists:filter(fun({_, Input1, Output1, _, _}) -> {Input1, Output1} == {Input, Output} end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
               lists:partition(fun({_, _, Input2, Output2, _, _}) -> {Input2, Output2} == {Input, Output} end, Current_rfps),
      case {Previous_rfp, Current_rfp} of
        {[], []} -> Rfp_no = Last_rfp + 1,
                    Pid ! {rfp_no, Rfp_no},
                    send_msg_to_list(Agent_list, {rfp, {Rfp_no, Submittor, Input, Output}}),
                    %% add problem to RFP list
                    Rfp_list = [{Rfp_no, Submittor, Input, Output, [], []} | Current_rfps],
                    loop(Agent_list, Rfp_no, Past_proposals, Rfp_list);
        {[], [{Rfp_no, Rfp_submittor, _, _, Agent_bids, Agent_nobids}]} ->
                   Rfp_list = Rem_Rfp_list ++ [{Rfp_no, Submittor ++ Rfp_submittor, Input, Output, Agent_bids, Agent_nobids}],
                   Pid ! {rfp_no, Rfp_no},
                   loop(Agent_list, Last_rfp, Past_proposals, Rfp_list);
        {[{Rfp_no, _, _, Services, Cost}], _} -> Pid ! {rfp_no, Rfp_no},
                                               send_msg_to_list(Submittor, {bid, {Rfp_no, Input, Output, Services, Cost}}),
                                               loop(Agent_list, Last_rfp, Past_proposals, Current_rfps)
      end;

    {bid, Proposal} ->
      %% 1)  Check past_proposals list for this rfp number.
      %% 2)  If it is not in the list, add it to the list and send the proposal to the submittor list.
      %% 3)  If it is in the list and the cost of this proposal is lower than the current one in the list,
      %%     update the list based on this proposal and send the new proposal to the submittor.
      %% 4)  Find the RFP in the Current_rfps list and indicate which agent provided a bid.
      {Rfp_no, Input, Output, Services, Cost} = Proposal,
      [Agent | _] = Services,
      {Previous_rfp, Prev_rfp_list} =
        lists:partition(fun({Rfp, _, _, _, _}) -> Rfp == Rfp_no end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({Rfp, _, _, _, _, _}) -> Rfp == Rfp_no end, Current_rfps),
      case {Previous_rfp, Current_rfp} of
        {[], []} ->
          %% Proposal is not a current RFP or a previous RFP --> ignore and process the next message
          loop(Agent_list, Last_rfp, Past_proposals, Current_rfps);
        {[], [{Rfp_no, Submittor, _, _, Agent_bids, Agent_nobids}]} ->
          Rfp_list = Rem_Rfp_list ++ [{Rfp_no, Submittor, Input, Output, Agent_bids ++ [Agent], Agent_nobids}],
          New_past_props = Prev_rfp_list ++ [Proposal],
          send_msg_to_list(Submittor, {bid, Proposal}),
          loop(Agent_list, Last_rfp, New_past_props, Rfp_list);
        {[{Rfp_no, _, _, _, Cost1}], [{_, Submittor, _, _, Agent_bids, Agent_nobids}]} ->
          Ccost1 = cost:calculate(Cost1),
          Ccost = cost:calculate(Cost),
          Rfp_list = case lists:member(Agent, Agent_bids) of
                       false -> Rem_Rfp_list ++ [{Rfp_no, Submittor, Input, Output, Agent_bids ++ [Agent], Agent_nobids}];
                       true -> Current_rfps
                     end,
          if
            Ccost1 > Ccost ->
              New_past_props = Prev_rfp_list ++ [Proposal],
              send_msg_to_list(Submittor, {bid, {Rfp_no, Input, Output, Services, Cost}}),
              loop(Agent_list, Last_rfp, New_past_props, Rfp_list);
            true ->
              loop(Agent_list, Last_rfp, Past_proposals, Rfp_list)
          end
      end;

      %% Rfp_list = update_rfp(Current_rfps, Proposal),
      %% loop(Agent_list, Last_rfp, Past_proposals, Rfp_list);

    {no_bid, {Name, Rfp_no}} ->
      %% 1)  Update the Rfp list that the specified agent has no bid.
      %% 2)  Check to see if all other agents have no bid.  If so send a no bid response to the submittor(s).
      %%  Rfp_list = no_bid(Name, Current_rfps, Rfp_no),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({Rfp, _, _, _, _, _}) -> Rfp == Rfp_no end, Current_rfps),
      [{_, Submittor, Input, Output, Agent_bids, Agent_nobids}] = Current_rfp,
      New_agent_nobids = Agent_nobids ++ [Name],
      Rfp_list = Rem_Rfp_list ++ [{Rfp_no, Submittor, Input, Output, Agent_bids, New_agent_nobids}],
      Rem_agents = lists:subtract(Agent_list, New_agent_nobids),
      if
        Rem_agents == [] -> send_msg_to_list(Submittor, {no_bid, Rfp_no});
        true -> true
      end,

      loop(Agent_list, Last_rfp, Past_proposals, Rfp_list);

    {status, Pid} ->
      Pid ! {Agent_list, Last_rfp, Past_proposals, Current_rfps},
      loop(Agent_list, Last_rfp, Past_proposals, Current_rfps);

    reset -> send_msg_to_list(Agent_list, reset),
             loop(Agent_list, 0, [], []);

    stop -> send_msg_to_list(Agent_list, stop),
            ok
  end.


send_msg_to_list([Pid | []], Info) ->
  Pid ! Info;
send_msg_to_list([Pid | T], Info) ->
  Pid ! Info,
  send_msg_to_list(T, Info).
