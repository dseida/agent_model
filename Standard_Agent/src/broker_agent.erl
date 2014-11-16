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
  %%  Problem = {Reply_to, Input, Output, Prior_Agents}
  %%  Prior_Agents = [] for non-agents;  Agents will add their name to the list
  broker_agent ! {rfp, Problem},
  receive
    {rfp_no, Broker_response} -> {rfp_no, Broker_response}
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
  loop(Agent_list, [], []).

loop(Agent_list, Past_proposals, Current_rfps) ->
  %%  Past_proposals = [{Input, Output, Services, Cost}, ... ]
  %%  Rfp_list = [{Reply_to, Prior_Agents, Input, Output, Agent_bids, Agent_nobids}, ...]
  %%  Reply_to = [ Pid, ...]
  %%  Prior_Agents = [ Agent_name, ...]
  %%  Input = [ ...]
  %%  Output = [ ... ]
  %%  Agent_bids = [ Agent_name, ...]
  %%  Services = [ AgentX, ...]
  %%  Cost = [ Cost_factor1, ...]
  %%  Agent_nobids = [ Agent_nameX, ...]
  receive
    %%  Problem = {Reply_to, Input, Output, Prior_Agents}
    {rfp, Problem} ->
      %% 1)  Check to see if the problem has already been solved
      %%     If so, return the RFP parameters and then mail the result to the requester
      %% 2)  Check to see if we are already working on the problem
      %%     If so, add the new submittor to the Reply_to list and acknowledge the rfp to new submittor
      %% 3)  Otherwise, add the problem to RFP_list, return the rfp acknowledgement, and send the problem to the agents
      {Reply_to, Input, Output, Prior_Agents} = Problem,
      Previous_rfp = lists:filter(fun({Input1, Output1, _, _}) -> {Input1, Output1} == {Input, Output} end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
               lists:partition(fun({_, _, Input2, Output2, _, _}) -> {Input2, Output2} == {Input, Output} end, Current_rfps),
      case {Previous_rfp, Current_rfp} of
        {[], []} -> send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output}}),
                    send_msg_to_list(Agent_list, {rfp, {{node(), broker_agent}, Input, Output, Prior_Agents}}),
                    %% add problem to RFP list
                    Rfp_list = [{Reply_to, Prior_Agents, Input, Output, [], []} | Current_rfps],
                    loop(Agent_list, Past_proposals, Rfp_list);
        {[], [{Reply_to1, Rfp_Prior_Agents, _, _, Agent_bids, Agent_nobids}]} ->
                   %% add function to deduplicate agents added to the prior_agents list
                   Rfp_list =  [{Reply_to1 ++ Reply_to, Prior_Agents ++ Rfp_Prior_Agents, Input, Output, Agent_bids, Agent_nobids} | Rem_Rfp_list],
                   send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output}}),
                   loop(Agent_list, Past_proposals, Rfp_list);
        {[{_, _, Services, Cost}], _} -> send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output}}),
                                               send_msg_to_list(Reply_to, {bid, {Input, Output, Services, Cost}}),
                                               loop(Agent_list, Past_proposals, Current_rfps)
      end;

    {bid, Proposal} ->
      %% 1)  Check past_proposals list for the input and output.
      %% 2)  If it is not in the list, add it to the list and send the proposal to the Reply_to list.
      %% 3)  If it is in the list and the cost of this proposal is lower than the current one in the list,
      %%     update the list based on this proposal and send the new proposal to the Reply_to list.
      %% 4)  Find the RFP in the Current_rfps list and indicate which agent provided a bid.
      %% {Input, Output, Services, Cost} = Proposal
      %% Rfp_list = [{Reply_to, Prior_Agents, Input, Output, Agent_bids, Agent_nobids}, ...]
      {Input, Output, Services, Cost} = Proposal,
      [Agent | _] = Services,
      {Previous_rfp, Prev_rfp_list} =
        lists:partition(fun({Input_old, Output_old, _, _}) -> {Input, Output} == {Input_old, Output_old} end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({_, _, Input_curr, Output_curr, _, _}) -> {Input, Output} == {Input_curr, Output_curr} end, Current_rfps),
      case {Previous_rfp, Current_rfp} of
        {[], []} ->
          %% Proposal is not a current RFP or a previous RFP --> ignore and process the next message
          loop(Agent_list, Past_proposals, Current_rfps);
        {[], [{Reply_to, Prior_Agents, _, _, Agent_bids, Agent_nobids}]} ->
          Rfp_list =  [{Reply_to, Prior_Agents, Input, Output, [Agent | Agent_bids], Agent_nobids} | Rem_Rfp_list],
          New_past_props =  [Proposal | Prev_rfp_list],
          send_msg_to_list(Reply_to, {bid, Proposal}),
          loop(Agent_list, New_past_props, Rfp_list);
        {[{ _, _, _, Cost1}], [{Reply_to, Prior_Agents, _, _, Agent_bids, Agent_nobids}]} ->
          Ccost1 = cost:calculate(Cost1),
          Ccost = cost:calculate(Cost),
          Rfp_list = case lists:member(Agent, Agent_bids) of
                       false -> [{Reply_to, Prior_Agents, Input, Output, [Agent | Agent_bids], Agent_nobids} | Rem_Rfp_list];
                       true -> Current_rfps
                     end,

          send_msg_to_list(Reply_to, {bid, {Input, Output, Services, Cost}}),
          if
            Ccost1 > Ccost ->
              New_past_props =  [Proposal | Prev_rfp_list],
              loop(Agent_list, New_past_props, Rfp_list);
            true ->
              loop(Agent_list, Past_proposals, Rfp_list)
          end
      end;

    {no_bid, {Name, {Input, Output}}} ->
      %% 1)  Update the Rfp list that the specified agent has no bid.
      %% 2)  Check to see if all other agents have no bid.  If so send a no bid response to the Reply_to list.
      %%  Rfp_list = no_bid(Name, Current_rfps, Rfp_no),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({_, _, Input1, Output1, _, _}) -> {Input1, Output1} == {Input, Output} end, Current_rfps),
      [{Reply_to, Prior_Agents, Input, Output, Agent_bids, Agent_nobids}] = Current_rfp,
      New_agent_nobids = [Name | Agent_nobids],
      Rfp_list =  [{Reply_to, Prior_Agents, Input, Output, Agent_bids, New_agent_nobids} | Rem_Rfp_list],
      Rem_agents = lists:subtract(Agent_list, New_agent_nobids),
      if
        Rem_agents == [] -> send_msg_to_list(Reply_to, {no_bid, {{node(), broker_agent},{Input, Output}}});
        true -> true
      end,

      loop(Agent_list, Past_proposals, Rfp_list);

    {status, Pid} ->
      send_msg_to_list([Pid], {Agent_list, Past_proposals, Current_rfps}),
      loop(Agent_list, Past_proposals, Current_rfps);

    reset -> send_msg_to_list(Agent_list, reset),
             loop(Agent_list, [], []);

    stop -> send_msg_to_list(Agent_list, stop),
            ok
  end.


send_msg_to_list([Pid | []], Info) ->
  Pid ! Info,
  {foo, foo@dlsMacAir} ! {Pid, Info};
send_msg_to_list([Pid | T], Info) ->
  Pid ! Info,
  {foo, foo@dlsMacAir } ! {Pid, Info},
  send_msg_to_list(T, Info).
