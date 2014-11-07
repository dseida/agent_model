%%%-------------------------------------------------------------------
%%% @author davidseida
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2014 7:53 AM
%%%-------------------------------------------------------------------
-module(cost).
-author("davidseida").

%% API
-export([add/2, calculate/1]).

%% add 2 lists of numbers where the lists are the same length
add([A | []], [B | []]) -> [A + B];

add([A | RemA], [B | RemB]) -> [ A + B | add(RemA, RemB)].

%% calculate the effective cost based on the list of cost factors
%% intially -> just add up a list of numbers of equal weight
calculate(A) -> lists:sum(A).

