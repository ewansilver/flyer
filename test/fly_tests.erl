%% Author: ewan
%% Created: 20 Nov 2012
%% Description: TODO: Add description to fly_tests
-module(fly_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

ensure_can_ping_test() ->
	{ok,_Pid} = fly:start(),
	["FlySpace"] = fly:ping(),
	shutdown_ok = fly:stop().

check_can_write_string_test() ->
	Typename = check_can_write_string_test,
	{ok,_Pid} = fly:start(),
	ok = fly:register(Typename,{string,string}),
	{ok,10000} = fly:write(Typename,{"name","ewan"},10000),
	shutdown_ok = fly:stop().

check_can_write_and_then_read_string_test() ->
	Typename = check_can_write_and_then_read_string_test,
	{ok,_Pid} = fly:start(),
	ok = fly:register(Typename,{string,string}),
	{ok,10000} = fly:write(Typename,{"name","ewan"},10000),
	{read,1,{"name","ewan"}} = fly:read(Typename,{"name",""}),
	shutdown_ok = fly:stop().

check_can_write_and_then_take_string_test() ->
	Typename = check_can_write_and_then_take_string_test,
	{ok,_Pid} = fly:start(),
	ok = fly:register(Typename,{string,string}),
	{ok,10000} = fly:write(Typename,{"name","ewan"},10000),
	{read,1,{"name","ewan"}} = fly:read(Typename,{"name",""}),
	{take,1,{"name","ewan"}} = fly:take(Typename,{"name",""}),
	{read,0,{}} = fly:read(Typename,{"name",""}),
	shutdown_ok = fly:stop().

check_an_empty_take_test_dontrun() ->
	Typename = check_an_empty_take_test,
	{ok,_Pid} = fly:start(),
	ok = fly:register(Typename,{string,string}),
	{take,0,{}} = fly:take(Typename,{"not","defined"}),
	shutdown_ok = fly:stop().

%%
%% Local Functions
%%

