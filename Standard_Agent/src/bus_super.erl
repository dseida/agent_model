%%%-------------------------------------------------------------------
%%% @author davidseida
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2015 12:24 PM
%%%-------------------------------------------------------------------
-module(bus_super).
-author("davidseida").

%% API
-export([start/1, stop/0, status/0, new_rfp/1, loop/1, send_msg/2, send_msg_to_list/2, start_agents/1]).

start(Config_no) ->
  Pid = spawn(bus_super, loop, [Config_no]),
  register(bus_super, Pid),
  ok.

%% Support testing from the shell
stop() ->
  bus_super ! stop,
  ok.

%% Support testing from the shell
%% Use flush to see the response
status() ->
  bus_super ! {status, self()},
  ok.

new_rfp(Problem) ->
  %% add logic to send {error, "Invalid Problem"} to calling routine if Problem does not meet the form {Submittor, Input, Output}
  %% where Input and Output are lists within the tuple
  %%  Problem = {Reply_to, Input, Output}
  %%  Prior_Nodes = [] for non-agents;  Agents will add their name to the list
  {Reply_to, Input, Output} = Problem,
  send_msg_to_list([bus_super] , {rfp, {Reply_to, Input, Output, [], [], []}}),
  receive
    {rfp_no, Bus_super_response} -> {rfp_no, Bus_super_response}
  after
    10000 -> {error, "No response from bus_super"}
  end.

start_agents(Config_no) ->
  {Agent_List, Bus_Super_List} = case Config_no of
    1 -> Config1 = {agent1_2, [1], [2], [1]},
      Config2 = {agent1_4, [1], [4], [1]},
      Config3 = {agent1_6, [1], [6], [1]},
      Config4 = {agent3_4, [3], [4], [1]},
      Config5 = {agent5_6, [5], [6], [1]},
      Config6 = {agent7_8, [7], [8], [1]},
      Config7 = {agent9_10, [9], [10], [1]},
      Config8 = {agent11_12, [11], [12], [1]},
      Config9 = {agent13_14, [13], [14], [1]},
      Config10 = {agent15_16, [15],[16], [4]},
      Config11 = {agent3_6, [3], [6], [2]},
      Config12 = {agent7_10, [7], [10], [10]},
      Config13 = {agent9_12, [9], [12], [3]},
      spawn(bus_agent, start, [Config1]),
      spawn(bus_agent, start, [Config2]),
      spawn(bus_agent, start, [Config3]),
      spawn(bus_agent, start, [Config4]),
      spawn(bus_agent, start, [Config5]),
      spawn(bus_agent, start, [Config6]),
      spawn(bus_agent, start, [Config7]),
      spawn(bus_agent, start, [Config8]),
      spawn(bus_agent, start, [Config9]),
      spawn(bus_agent, start, [Config10]),
      spawn(bus_agent, start, [Config11]),
      spawn(bus_agent, start, [Config12]),
      spawn(bus_agent, start, [Config13]),
      {[{agent1_2, prime@dlsMacAir}, {agent1_4, prime@dlsMacAir},
        {agent1_6, prime@dlsMacAir}, {agent3_4, prime@dlsMacAir},
        {agent5_6, prime@dlsMacAir}, {agent7_8, prime@dlsMacAir},
        {agent9_10, prime@dlsMacAir}, {agent11_12, prime@dlsMacAir},
        {agent13_14, prime@dlsMacAir}, {agent15_16, prime@dlsMacAir},
        {agent3_6, prime@dlsMacAir}, {agent7_10, prime@dlsMacAir},
        {agent9_12, prime@dlsMacAir}],
        [{bus_super, test@dlsMacAir}, {bus_super, test1@dlsMacAir}]};
    2 -> Config1 = {agent2_3, [2], [3], [1]},
      Config2 = {agent4_5, [4], [5], [1]},
      Config3 = {agent6_7, [6], [7], [1]},
      Config4 = {agent8_9, [8], [9], [1]},
      Config5 = {agent10_11, [10], [11], [1]},
      Config6 = {agent12_13, [12], [13], [1]},
      Config7 = {agent14_15, [14], [15], [1]},
      Config8 = {agent2_5, [2], [5], [1]},
      Config9 = {agent6_9, [6], [9], [1]},
      Config10 = {agent2_11, [2],[11], [4]},
      spawn(bus_agent, start, [Config1]),
      spawn(bus_agent, start, [Config2]),
      spawn(bus_agent, start, [Config3]),
      spawn(bus_agent, start, [Config4]),
      spawn(bus_agent, start, [Config5]),
      spawn(bus_agent, start, [Config6]),
      spawn(bus_agent, start, [Config7]),
      spawn(bus_agent, start, [Config8]),
      spawn(bus_agent, start, [Config9]),
      spawn(bus_agent, start, [Config10]),
      {[{agent2_3, test@dlsMacAir}, {agent4_5, test@dlsMacAir},
        {agent6_7, test@dlsMacAir}, {agent8_9, test@dlsMacAir},
        {agent10_11, test@dlsMacAir}, {agent12_13, test@dlsMacAir},
        {agent14_15, test@dlsMacAir}, {agent2_5, test@dlsMacAir},
        {agent6_9, test@dlsMacAir}, {agent2_11, test@dlsMacAir}],
        [{bus_super, prime@dlsMacAir}, {bus_super, test1@dlsMacAir}]};
    _ -> Config1 = {agent1_14, [1], [14], [7]},
      Config2 = {agent3_12, [3], [12], [9]},
      Config3 = {agent3_7, [3], [7], [1]},
      Config4 = {agent5_9, [5], [9], [2]},
      Config5 = {agent7_14, [7], [14], [3]},
      Config6 = {agent1_10, [1], [10], [8]},
      Config7 = {agent12_15, [12], [15], [1]},
      Config8 = {agent12_17, [12], [17], [1]},
      Config9 = {agent12_19, [12], [19], [1]},
      Config10 = {agent12_1, [12],[1], [4]},
      spawn(bus_agent, start, [Config1]),
      spawn(bus_agent, start, [Config2]),
      spawn(bus_agent, start, [Config3]),
      spawn(bus_agent, start, [Config4]),
      spawn(bus_agent, start, [Config5]),
      spawn(bus_agent, start, [Config6]),
      spawn(bus_agent, start, [Config7]),
      spawn(bus_agent, start, [Config8]),
      spawn(bus_agent, start, [Config9]),
      spawn(bus_agent, start, [Config10]),
      {[{agent1_14, test1@dlsMacAir}, {agent3_12, test1@dlsMacAir},
        {agent3_7, test1@dlsMacAir}, {agent5_9, test1@dlsMacAir},
        {agent7_14, test1@dlsMacAir}, {agent1_10, test1@dlsMacAir},
        {agent12_15, test1@dlsMacAir}, {agent12_17, test1@dlsMacAir},
        {agent12_19, test1@dlsMacAir}, {agent12_1, test1@dlsMacAir}],
        [{bus_super, prime@dlsMacAir}, {bus_super, test1@dlsMacAir}]}
  end,
  ets:new(local_agents, [bag, named_table]),
  ets:new(bus_controllers, [bag, named_table]),
  ets:insert(local_agents, Agent_List),
  ets:insert(bus_controllers, Bus_Super_List),
  {Agent_List, Bus_Super_List}.

loop(Config_no) ->
  {Agent_list, Bus_Super_List} = start_agents(Config_no),
  loop(Agent_list, Bus_Super_List).

loop(Agent_list, Hosts_on_bus) ->
  receive
  %%  Problem = {Reply_to, Input, Output, Agent_List, Prior_Nodes, Cost}
    {rfp, Problem} ->
      %% Forward to all agents
        {Reply_to, _Input, _Output, _Agent_List, _Prior_Nodes, _Cost} = Problem,
        send_msg_to_list(Reply_to, {rfp_no, {"Problem received", Problem}}),
        send_msg(bus_super, {rfp, Problem}),
        loop(Agent_list, Hosts_on_bus);

    {bus_message, Message} ->
        send_msg_to_list(Agent_list, Message),
        loop(Agent_list, Hosts_on_bus);

    {status, Pid} ->
      send_msg_to_list([Pid], {Agent_list, Hosts_on_bus}),
      loop(Agent_list, Hosts_on_bus);

    stop -> send_msg_to_list(Agent_list, stop),
      ok
  end.


send_msg_to_list([], _) ->
  [];
send_msg_to_list([Pid | []], Info) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", Pid}, Info},
  Pid ! Info;
send_msg_to_list([Pid | T], Info) ->
  {recorder, foo@dlsMacAir } ! {{"From:", {self(), node()}}, {"To:", Pid}, Info},
  Pid ! Info,
  send_msg_to_list(T, Info).

send_msg(Sender, Message) ->
  send_msg_local(Sender, Message),
  send_msg_remote(Message).

send_msg_local(Sender, Message) ->
  First_key = ets:first(local_agents),
  send_msg_local(Sender, Message, First_key).

send_msg_local(_Sender, _Message, '$end_of_table') ->
  [];

send_msg_local({Sender_key, Host}, Message, Sender_key) ->
  Sender = {Sender_key, Host},
  send_msg_local( Sender, Message, ets:next(local_agents, Sender_key));

send_msg_local(Sender, Message, Agent_key) ->
  Agent = ets:lookup(local_agents, Agent_key),
  send_msg_to_list(Agent, Message),
  send_msg_local( Sender, Message, ets:next(local_agents, Agent_key)).

send_msg_remote(Message) ->
  Remote_hosts_key = ets:first(bus_controllers),
  Remote_hosts_list = ets:lookup(bus_controllers, Remote_hosts_key),
  send_msg_to_list(Remote_hosts_list, {bus_message, Message}).
