%%%-------------------------------------------------------------------
%%% @author davidseida
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2014 11:53 AM
%%%-------------------------------------------------------------------
-module(metrics).
-author("davidseida").

%% API
-export([start/0, stop/0, status/0, reset/0, loop/1]).

%% Receive copies of messages between agents and count up the messages
%% Runs on its own node:  foo@"machine_name"

start() ->
  Pid = spawn(metrics, loop, [[{cross_node, 0}, {intra_node, 0}]]),
  register(recorder, Pid),
  ok.

%% Support testing from the shell
stop() ->
  recorder ! stop,
  ok.

reset() ->
  recorder ! reset,
  ok.

%% Support testing from the shell
%% Use flush to see the response
status() ->
  recorder ! {status, self()},
  receive
    {status_reply, Recorder_response} -> {status_reply, Recorder_response}
  after
    10000 -> {error, "No response from metrics recorder"}
  end.

loop(Stats) ->
  receive
    stop -> ok;
    {status, Pid} ->
      Pid ! {status_reply, Stats},
      loop(Stats);
    reset -> loop([{cross_node, 0}, {intra_node, 0}]);
    {From_Address, To_Address, _Message} ->
      {"From:", {_Pid_from, Node_from}} = From_Address,
      {"To:", Address_to} = To_Address,
      Cross_node = if
        is_tuple(Address_to) ->  {_, Node_to} = Address_to,
                                 Node_from /= Node_to;
        true -> false
      end,
      [{cross_node, CN_count}, {intra_node, IN_count}] = Stats,
      New_stats = case Cross_node of
        true -> [{cross_node, CN_count + 1}, {intra_node, IN_count}];
        false -> [{cross_node, CN_count}, {intra_node, IN_count + 1}]
      end,
      loop(New_stats)

  end.
