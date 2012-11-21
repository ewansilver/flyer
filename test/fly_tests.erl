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

check_an_empty_take_test() ->
	Typename = fly_tests_check_an_empty_take_test,
	{ok,_Pid} = fly:start(),
	ok = fly:register(Typename,{string,string}),
	{take,0,{}} = fly:take(Typename,{"not","defined"}),
	shutdown_ok = fly:stop().


big_test() ->
	{ok,_Pid} = fly:start(),
%% The fact we have to call type_structrure_preamble is a hack at the moment. Need to find someway
%% to hide this inside the returned State reference or something so that we can start to actually
%% return the right kind of tuples.
	Person = person,
	ok = fly:register(Person,[{string,"name"},{string,"surname"},{int,"age"}, {atom,"sex"}]),

	{ok,1000} = fly:write(Person,{"Monty","Silver",2, male},1000),
	{ok,1001} = fly:write(Person,{"Elliot","Silver",6, male},1001),

	{read_many,2,[_,_]} = fly:read_many(Person, {"","Silver","",""},10),	

	{read,1,{"Monty","Silver",2,male}} = fly:read(Person, {"Monty","","",""}),
	{read,1,{"Elliot","Silver",6,male}} = fly:read(Person, {"","",6,""}),

	{take,1,{"Monty","Silver",2,male}} = fly:take( Person, {"Monty","",""}),
	{take,1,{"Elliot","Silver",6,male}} = fly:take( Person, {"","",6,""}),

%% Check that the space is now empty again. Use all available checking calls.	
	{take,0,{}} = fly:take(Person,{"","Silver","",""}),
	{read,0,{}} = fly:read(Person,{"","Silver","",""}),
	{read_many,0,[]} = fly:read_many(Person, {"","Silver","",""},10),	
	
%% Finally we want to check that we can take multiple entries in one go.
	{ok,1000} = fly:write( Person,{"Monty","Silver",2,""},1000),
	{ok,1001} = fly:write( Person,{"Elliot","Silver",6,""},1001),
	{read_many,2,[_,_]} = fly:read_many( Person, {"","Silver","",""},10),	
	{take_many,2,[_,_]} = fly:take_many( Person, {"","Silver","",""},10),	
	{take,0,{}} = fly:take( Person,{"","Silver","",""}),
	{read,0,{}} = fly:read( Person,{"","Silver","",""}),
	{read_many,0,[]} = fly:read_many( Person, {"","Silver","",""},10),
	{take_many,0,[]} = fly:take_many( Person, {"","Silver","",""},10),	
	
	shutdown_ok = fly:stop().
