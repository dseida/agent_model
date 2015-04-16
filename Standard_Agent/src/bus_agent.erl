%%%-------------------------------------------------------------------
%%% @author davidseida
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2015 1:10 PM
%%%-------------------------------------------------------------------
-module(bus_agent).
-author("davidseida").

%% API
-export([start/1, stop/1, status/1, loop/1]).


%% start, stop, status are external access methods
%% all other methods are used internally and are not directly accessible
%%  {Name, Agent_In, Agent_Out, Cost} = Config
start(Config) ->
  {Name, _, _, _} = Config,
  Pid = spawn(standard_agent, loop, [Config]),
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

loop(Config) ->
  receive
  %%  {Reply_to, Input, Output, Agent_List, Prior_Nodes, Cost_Vec} = Cand_Rfp
    {rfp, Cand_Rfp} ->
      review_rfp(Config, Cand_Rfp),
      loop(Config);
    {status, Pid} ->
      send_msg(Pid, Config),
      loop(Config);
    reset -> loop(Config);
    stop -> ok
  end.

%%  review_rfp cases
%%  1)  Fresh rfp (Empty Agent_List, Empty Prior_Nodes), rfp input and output match agent input and output
%%      Agent puts its name in the Agent_List, pushes its input and output into the Prior_Nodes list,
%%      provides it cost and sends its bid to the Reply_to
%%  2)  Fresh rfp, rfp input matches agent, but rfp output does not match agent output
%%      Agent puts its name in the Agent_List, pushes its input and output into the Prior_Nodes list,
%%      Adds in its cost (TBD), and sends out the RFP with updated cost, Agent_List and Prior_Nodes
%%  3)  Updated rfp, first element of Prior_Nodes matches agent input and agent output matches rfp output
%%      Agent puts its name in the Agent_List, adds its cost to the current cost and sends its bid to the Reply_to
%%  4)  Updated rfp, first element of Prior_Nodes matches agent input and agent output not in Prior_Nodes
%%      Agent puts its name in the Agent_List, pushes its output into the Prior_Nodes list,
%%      Adds in its cost (TBD), and sends out the RFP with updated cost, Agent_List and Prior_Nodes
%%  5)  Otherwise, ignore


%% Case 1
review_rfp({Name, In, Out, Cost}, {Reply_to, In, Out, [], [], _}) ->
  send_msg(Reply_to, {bid, {In, Out, [Name], [Out, In], Cost}}),
  [];

%% Case 2
review_rfp({Name, In, Agent_Out, Cost}, {Reply_to, In, Rfp_Out, [], [], _}) ->
  send_msg({rfp, {Reply_to, In, Rfp_Out, [Name], [Agent_Out, In], Cost}}),
  [];

%% Case 3
review_rfp({Name, Agent_In, Out, Cost}, {Reply_to, In, Out, Agent_List, [Agent_In | Prior_Nodes], Cost_Vec}) ->
  send_msg(Reply_to, {bid, {In, Out, [Name | Agent_List], [Out | [In | Prior_Nodes]], cost:add(Cost, Cost_Vec)}}),
  [];

%% Case 4
review_rfp({Name, Agent_In, Agent_Out, Cost}, {Reply_to, In, Out, Agent_List, [Agent_In | Prior_Nodes], Cost_Vec}) ->
  case lists:member(Agent_Out, Prior_Nodes) of
    true ->
      [];
    false ->
      send_msg({rfp, {Reply_to, In, Out, [Name |Agent_List], [Agent_Out | [Agent_In | Prior_Nodes]], cost:add(Cost, Cost_Vec)}})
  end,
  [];

%% Case 5
review_rfp({_Name, _, _, _}, {_, _In, _Out, _Prior_Nodes}) ->
  [].


send_msg(Message) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", broker_agent}, Message},
  broker_agent ! Message.

send_msg(Pid, Message) ->
  {recorder, foo@dlsMacAir} ! {{"From:", {self(), node()}}, {"To:", Pid}, Message},
  Pid ! Message.
