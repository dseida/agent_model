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
-export([start/1, stop/0, status/0, reset/0, new_rfp/1, loop/1, start_agents/1, best_current/1]).

start(Config_no) ->
  Pid = spawn(broker_agent, loop, [Config_no]),
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
  %%  Problem = {Reply_to, Input, Output, Prior_Nodes}
  %%  Prior_Nodes = [] for non-agents;  Agents will add their name to the list
  send_msg_to_list([broker_agent] , {rfp, Problem}),
  receive
    {rfp_no, Broker_response} -> {rfp_no, Broker_response}
  after
    10000 -> {error, "No response from broker_agent"}
  end.

best_current(Problem) ->
  send_msg_to_list([broker_agent] , {best_current, Problem}).


start_agents(Config_no) ->
  case Config_no of
    1 -> Config1 = {agent1, [1], [2], [1]},
         Config2 = {agent2, [1], [4], [1]},
         Config3 = {agent3, [1], [6], [1]},
         Config4 = {agent4, [3], [4], [1]},
         Config5 = {agent5, [5], [6], [1]},
         Config6 = {agent6, [7], [8], [1]},
         Config7 = {agent7, [9], [10], [1]},
         Config8 = {agent8, [11], [12], [1]},
         Config9 = {agent9, [13], [14], [1]},
         Config10 = {agent10, [15],[16], [4]},
         Config11 = {agent11, [3], [6], [2]},
         Config12 = {agent12, [7], [10], [10]},
         Config13 = {agent13, [9], [12], [3]},
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
         [agent1, agent2, agent3, agent4, agent5, agent6, agent7, agent8, agent9, agent10,
           agent11, agent12, agent13, {broker_agent, test@dlsMacAir}, {broker_agent, test1@dlsMacAir}];
    2 -> Config1 = {agent14, [2], [3], [1]},
         Config2 = {agent15, [4], [5], [1]},
         Config3 = {agent16, [6], [7], [1]},
         Config4 = {agent17, [8], [9], [1]},
         Config5 = {agent18, [10], [11], [1]},
         Config6 = {agent19, [12], [13], [1]},
         Config7 = {agent20, [14], [15], [1]},
         Config8 = {agent21, [2], [5], [1]},
         Config9 = {agent22, [6], [9], [1]},
         Config10 = {agent23, [2],[11], [4]},
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
         [agent14, agent15, agent16, agent17, agent18, agent19, agent20, agent21, agent22, agent23, {broker_agent, prime@dlsMacAir}];
    _ -> Config1 = {agent24, [1], [14], [7]},
      Config2 = {agent25, [3], [12], [9]},
      Config3 = {agent26, [3], [7], [1]},
      Config4 = {agent27, [5], [9], [2]},
      Config5 = {agent28, [7], [14], [3]},
      Config6 = {agent29, [1], [10], [8]},
      Config7 = {agent30, [12], [15], [1]},
      Config8 = {agent31, [12], [17], [1]},
      Config9 = {agent32, [12], [19], [1]},
      Config10 = {agent33, [12],[1], [4]},
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
      [agent24, agent25, agent26, agent27, agent28, agent29, agent30, agent31, agent32, agent33, {broker_agent, prime@dlsMacAir}]


  end.

loop(Config_no) ->
  Agent_list = start_agents(Config_no),
  loop(Agent_list, [], []).

loop(Agent_list, Past_proposals, Current_rfps) ->
  %%  Past_proposals = [{Input, Output, Prior_Nodes, Services, Cost}, ... ]
  %%  Rfp_list = [{Reply_to, Prior_Nodes, Input, Output, Agent_bids, Agent_nobids}, ...]
  %%  Reply_to = [ Pid, ...]
  %%  Prior_Nodes = [ Node_Desc, ...] -> currently order specific -> add lists:reverse(lists:usort(Prior_Nodes))
  %%  Input = [ In_Node_Desc ]
  %%  Output = [ Out_Node_Desc ]
  %%  Agent_bids = [ Agent_name, ...]
  %%  Services = [ AgentX, ...]
  %%  Cost = [ Cost_factor1, ...]
  %%  Agent_nobids = [ Agent_nameX, ...]
  receive
    %%  Problem = {Reply_to, Input, Output, Prior_Nodes}
    {rfp, Problem} ->
      %% 1)  Check to see if the problem has already been solved
      %%     If so, return the RFP parameters and then mail the result to the requester
      %% 2)  Check to see if we are already working on the problem
      %%     If so, add the new submittor to the Reply_to list and acknowledge the rfp to new submittor
      %% 3)  Otherwise, add the problem to RFP_list, return the rfp acknowledgement, and send the problem to the agents
      {Reply_to, Input, Output, Prior_Nodes} = Problem,
      Previous_rfp = lists:filter(fun({Input1, Output1, Prior_Nodes1, _, _}) ->
               {Input1, Output1, Prior_Nodes1} == {Input, Output, Prior_Nodes} end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
               lists:partition(fun({_, Prior_Nodes2, Input2, Output2, _, _}) ->
                 {Input2, Output2, Prior_Nodes2} == {Input, Output, Prior_Nodes} end, Current_rfps),
      Eff_Agent_List = broker_filter(Agent_list, Reply_to),
      case {Previous_rfp, Current_rfp} of
        {[], []} -> send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output, Prior_Nodes}}),
                    send_msg_to_list(Eff_Agent_List, {rfp, {[{broker_agent ,node() }], Input, Output, Prior_Nodes}}),
                    %% add problem to RFP list
                    Rfp_list = [{Reply_to, Prior_Nodes, Input, Output, [],
                      lists:usort(lists:subtract(Agent_list, Eff_Agent_List))} | Current_rfps],
                    loop(Agent_list, Past_proposals, Rfp_list);
        {[], [{Reply_to1, Rfp_Prior_Nodes, _, _, Agent_bids, Agent_nobids}]} ->
                   %% add function to deduplicate agents added to the Prior_Nodes list
                   New_agent_nobids = lists:usort(lists:subtract(Agent_list, Eff_Agent_List) ++ Agent_nobids),
                   Rfp_list =  [{lists:usort(Reply_to1 ++ Reply_to), Rfp_Prior_Nodes,
                                  Input, Output, Agent_bids, New_agent_nobids} | Rem_Rfp_list],
                   send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output, Prior_Nodes}}),
                   Rem_agents = lists:subtract(Agent_list, New_agent_nobids),
                   if
                      Rem_agents == [] -> send_msg_to_list(Reply_to, {no_bid, {{broker_agent, node()},{Input, Output, Prior_Nodes}}});
                      true -> true
                   end,
                   loop(Agent_list, Past_proposals, Rfp_list);
        {[{_, _, _, Services, Cost}], _} -> send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Input, Output, Prior_Nodes}}),
                                               send_msg_to_list(Reply_to, {bid, {Input, Output, Prior_Nodes, Services, Cost}}),
                                               loop(Agent_list, Past_proposals, Current_rfps)
      end;

    {best_current, Problem} ->
      %% 1)  Check to see if the problem has already been solved
      %%     If so, return the RFP parameters and then mail the result to the requester
      %% 2)  Check to see if we are already working on the problem
      %%     If so, add the new submittor to the Reply_to list and acknowledge the rfp to new submittor
      %% 3)  Otherwise, add the problem to RFP_list, return the rfp acknowledgement, and send the problem to the agents
      {Reply_to, Input, Output, Prior_Nodes} = Problem,
      Previous_props = lists:filter(fun({Input1, Output1, _, _, _}) ->
        {Input1, Output1} == {Input, Output} end, Past_proposals),
      Best_prior = search_prior_results( Previous_props, Prior_Nodes),
      send_msg_to_list(Reply_to, Best_prior),
      loop(Agent_list, Past_proposals, Current_rfps);



    {bid, Proposal} ->
      %% 1)  Check past_proposals list for the input and output.
      %% 2)  If it is not in the list, add it to the list and send the proposal to the Reply_to list.
      %% 3)  If it is in the list and the cost of this proposal is lower than the current one in the list,
      %%     update the list based on this proposal and send the new proposal to the Reply_to list.
      %% 4)  Find the RFP in the Current_rfps list and indicate which agent provided a bid.
      %% {Input, Output, Services, Cost} = Proposal
      %% Rfp_list = [{Reply_to, Prior_Nodes, Input, Output, Agent_bids, Agent_nobids}, ...]
      {Input, Output, Prior_Nodes, Services, Cost} = Proposal,
      [Agent | _] = Services,
      {Previous_rfp, Prev_rfp_list} =
        lists:partition(fun({Input_old, Output_old, Prior_Nodes_old, _, _}) ->
          {Input, Output, Prior_Nodes} == {Input_old, Output_old, Prior_Nodes_old} end, Past_proposals),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({_, Prior_Nodes_curr, Input_curr, Output_curr, _, _}) ->
          {Input, Output, Prior_Nodes} == {Input_curr, Output_curr, Prior_Nodes_curr} end, Current_rfps),
      case {Previous_rfp, Current_rfp} of
        {[], []} ->
          %% Proposal is not a current RFP or a previous RFP --> ignore and process the next message
          loop(Agent_list, Past_proposals, Current_rfps);
        {[], [{Reply_to, Prior_Nodes, _, _, Agent_bids, Agent_nobids}]} ->
          Rfp_list =  [{Reply_to, Prior_Nodes, Input, Output, [Agent | Agent_bids], Agent_nobids} | Rem_Rfp_list],
          New_past_props =  [Proposal | Prev_rfp_list],
          send_msg_to_list(Reply_to, {bid, Proposal}),
          loop(Agent_list, New_past_props, Rfp_list);
        {[{ _, _,  _, _, Cost1}], [{Reply_to, Prior_Nodes, _, _, Agent_bids, Agent_nobids}]} ->
          Ccost1 = cost:calculate(Cost1),
          Ccost = cost:calculate(Cost),
          Rfp_list = case lists:member(Agent, Agent_bids) of
                       false -> [{Reply_to, Prior_Nodes, Input, Output, [Agent | Agent_bids], Agent_nobids} | Rem_Rfp_list];
                       true -> Current_rfps
                     end,

          send_msg_to_list(Reply_to, {bid, {Input, Output, Prior_Nodes, Services, Cost}}),
          if
            Ccost1 > Ccost ->
              New_past_props =  [Proposal | Prev_rfp_list],
              loop(Agent_list, New_past_props, Rfp_list);
            true ->
              loop(Agent_list, Past_proposals, Rfp_list)
          end
      end;

    {no_bid, {Name, {Input, Output, Prior_Nodes}}} ->
      %% 1)  Update the Rfp list that the specified agent has no bid.
      %% 2)  Check to see if all other agents have no bid.  If so send a no bid response to the Reply_to list.
      %%  Rfp_list = no_bid(Name, Current_rfps, Rfp_no),
      {Current_rfp, Rem_Rfp_list} =
        lists:partition(fun({_, Prior_Nodes1, Input1, Output1, _, _}) ->
          {Input1, Output1, Prior_Nodes1} == {Input, Output, Prior_Nodes} end, Current_rfps),
      [{Reply_to, Prior_Nodes, Input, Output, Agent_bids, Agent_nobids}] = Current_rfp,
      New_agent_nobids = lists:usort([Name | Agent_nobids]),
      Rfp_list =  [{Reply_to, Prior_Nodes, Input, Output, Agent_bids, New_agent_nobids} | Rem_Rfp_list],
      Rem_agents = lists:subtract(Agent_list, New_agent_nobids),
      if
        Rem_agents == [] -> send_msg_to_list(Reply_to, {no_bid, {{broker_agent, node()},{Input, Output, Prior_Nodes}}});
        true -> true
      end,

      loop(Agent_list, Past_proposals, Rfp_list);

    {status, Pid} ->
      send_msg_to_list([Pid], {Agent_list, Past_proposals, Current_rfps}),
      loop(Agent_list, Past_proposals, Current_rfps);

    reset -> case {Past_proposals, Current_rfps} of
               {[], []} -> loop(Agent_list, [], []);
               {_, _} -> send_msg_to_list(Agent_list, reset),
                         loop(Agent_list, [], [])
                 end;

    stop -> send_msg_to_list(Agent_list, stop),
           {foo, foo@dlsMacAir} ! {node(), Past_proposals, Current_rfps},
            ok
  end.

search_prior_results( [], _Prior_Nodes) ->
  "best_current - No paths currently known";
search_prior_results( Previous_props, Prior_Nodes) ->
  search_prior_results( Previous_props, Prior_Nodes, []).

search_prior_results( [], _Prior_Nodes, []) ->
  "best_current - No paths currently known";
search_prior_results( [], _Prior_Nodes, [Best_answer]) ->
  Best_answer;
search_prior_results( [Curr_bid | Rem_props], Prior_Nodes, [] ) ->
  {_, _, Bid_prior_nodes, _, _} = Curr_bid,
  Node_delta = lists:subtract(Prior_Nodes, Bid_prior_nodes),
  case Node_delta of
    [] -> search_prior_results(Rem_props, Prior_Nodes, [Curr_bid]);
    _ -> search_prior_results(Rem_props, Prior_Nodes, [])
  end;
search_prior_results( [Curr_bid | Rem_props], Prior_Nodes, [Curr_best]) ->
  {_, _, Bid_prior_nodes, _, Curr_cost} = Curr_bid,
  {_, _, _, _, Curr_best_cost} = Curr_best,
  Node_delta = lists:subtract(Prior_Nodes, Bid_prior_nodes),
  case Node_delta of
    [] ->
      Curr_cost_val = cost:calculate(Curr_cost),
      Curr_best_cost_val = cost:calculate(Curr_best_cost),
      Bid_choice = if
                     Curr_cost_val < Curr_best_cost_val -> Curr_bid;
                     true -> Curr_best
                   end,
      search_prior_results(Rem_props, Prior_Nodes, [Bid_choice]);
    _ -> search_prior_results(Rem_props, Prior_Nodes, [Curr_best])
  end.



broker_filter(Agent_list, [Pid | []]) ->
  lists:filter(fun(X) -> X /= Pid end, Agent_list);
broker_filter(Agent_list, [Pid | Reply_to]) ->
  New_Agent_list = lists:filter(fun(X) -> X /= Pid end, Agent_list),
  broker_filter(New_Agent_list, Reply_to).

send_msg_to_list([Pid | []], Info) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", Pid}, Info},
  Pid ! Info;
send_msg_to_list([Pid | T], Info) ->
  {recorder, foo@dlsMacAir } ! {{"From:", {self(), node()}}, {"To:", Pid}, Info},
  Pid ! Info,
  send_msg_to_list(T, Info).
