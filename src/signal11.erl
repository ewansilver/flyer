%% Author: ewan
%% Description: Demonstrate how to cause a signal 11 fault on Fly v2.
%% It happily registers a typename with one field definition (in this case a tuple with a single int)
%% and then faults when I try to register a different field defintion e.g. a tuple with two ints.

-module(signal11).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%

test() ->
	{ok,_Pid} = fly:start(),
	Typename = any_typename,
	ok = fly:register(Typename,{int}),
	ok = fly:register(Typename,{int,int}).

%%
%% Local Functions
%%

