%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010 Ewan Silver.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% Author: Ewan Silver
%% Created: 27 Apr 2010
%% Description: Fly Eunit test module.

-module(fly_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%%% 
%% Tests
%%%%

test_connect() ->
	fly:connect().

%% Test
check_can_ping_a_fly_instance_test() ->
	State = test_connect(),
	Tags = fly:ping(State),
	io:fwrite("Tags: ~p~n",[Tags]),
	fly:close(State).

ensure_a_missing_type_fails_test() ->
	State = test_connect(),
	
		%% Check what happens if ask for an entry type that is not present
	{fail, missing_type} = fly:write(State, missing,{"Ewan","Silver",100, male},1000),
	{fail, missing_type} = fly:read(State, missing,{"","",100, ""},1000),
	{fail, missing_type} = fly:take(State, missing,{"","",100, ""},1000),
	{fail, missing_type} = fly:read_many(State,missing, {"","",100,""},10),	
	{fail, missing_type} = fly:take_many(State,missing, {"","",100,""},10),
	fly:close(State).
	
check_can_write_read_and_take_all_formats_test() ->
	%% Try out the alternative registration mechanism	
	Type = check_can_write_read_and_take_all_formats_test,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{string,string,int,atom}}),

	{write,1000} = fly:write(State1, Type,{"Ewan","Silver",100, male},1000),	
	{write,1000} = fly:write(State1, Type,{"Alison","Silver",100, female},1000),
	{read,1,{"Ewan","Silver",100,male}} = fly:read(State1, Type, {"Ewan","","",""}, 1000),
	{read,1,{"Ewan","Silver",100,male}} = fly:read(State1, Type, {"","",100,""}, 1000),
	{read,1,{"Alison","Silver",100,female}} = fly:read(State1, Type, {"","","",female}, 1000),
	
	{read_many,2,ReadList} = fly:read_many(State1,Type, {"","",100,""},10),	
	{take_many,2,TakeList} = fly:take_many(State1,Type, {"","",100,""},10),

	?assert(length(ReadList) == 2),
	?assert(contains({"Ewan","Silver",100, male},ReadList)),
	?assert(contains({"Alison","Silver",100, female},ReadList)),

	?assert(length(TakeList) == 2),
	?assert(contains({"Ewan","Silver",100, male},TakeList)),
	?assert(contains({"Alison","Silver",100, female},TakeList)),
	
	{read_many,0,[]} = fly:read_many(State1,Type, {"","",100,""},10),
	{take_many,0,[]} = fly:take_many(State1,Type, {"","",100,""},10),
	fly:close(State1).

check_can_mimic_a_blocking_read_test() ->
	Type = check_can_mimic_a_blocking_read_test,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{string,string,int,atom}}),
		%% Check that we can mimic a blocking read and take 
	Pause=500,
	Now  = erlang:now(),
	{take,0,{}} = fly:take(State1,Type, {"","",100,""},Pause),
	true = Pause*1000 < timer:now_diff(erlang:now(),Now),
	fly:close(State1).

check_an_empty_read_test() ->
		%% Ensure that an empty read is properly handled.
	Type = check_an_empty_read_test,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{atom,tuple}}),
	{read,0,{}} = fly:read(State1, Type, {test,""}, 100),
	fly:close(State1).

check_an_empty_take_test() ->
		%% Ensure that an empty take is properly handled.
	Type = check_an_empty_take_test,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{atom,tuple}}),
	{take,0,{}} = fly:take(State1, Type, {test,""}, 100),
	fly:close(State1).

check_we_can_use_a_generic_tuple_test() ->
		%% Check we can send in a generic tuple
	Type = generic_tuple,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{atom,tuple}}),
	{write,1000} = fly:write(State1, Type,{test,{a,tuple,"of",[1,2,4,"things"]}},1000),
	{read,1,{test,{a,tuple,"of",[1,2,4,"things"]}}} = fly:read(State1, Type, {test,""}, 1000),
	{read,1,{test,{a,tuple,"of",[1,2,4,"things"]}}} = fly:read(State1, Type, {"",{a,tuple,"of",[1,2,4,"things"]}}, 1000),
	{take,1,{test,{a,tuple,"of",[1,2,4,"things"]}}} = fly:take(State1, Type, {test,""}, 1000),
	fly:close(State1).

check_we_can_use_a_generic_list_test() ->
		%% Check we can send in a list tuple
	Type = generic_list,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{atom,list}}),
	List = [1,2,3],
	{write,1000} = fly:write(State1, Type,{test,List},1000),	
	{read,1,{test,List}} = fly:read(State1, Type, {test,""}, 1000),
	{read,1,{test,List}} = fly:read(State1, Type, {"",List}, 1000),
	{take,1,{test,List}} = fly:take(State1, Type, {test,""}, 1000),
	fly:close(State1).

check_we_can_use_a_generic_binary_test() ->
	Type = binary_read,
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{Type,{atom,binary}}),
	Binary = term_to_binary(arandomterm),
	{read,0,{}} = fly:read(State1, Type, {binarytest,""}, 1000),
	{write,1000} = fly:write(State1, Type,{binarytest,Binary},1000),	
	{read,1,{binarytest,Binary}} = fly:read(State1, Type, {binarytest,""}, 1000),
	{read,1,{binarytest,Binary}} = fly:read(State1, Type, {"",Binary}, 1000),
	{take,1,{binarytest,Binary}} = fly:take(State1, Type, {binarytest,""}, 1000),
	{read,0,{}} = fly:read(State1, Type, {binarytest,""}, 1000),
	fly:close(State1).

	
check_that_trying_to_register_a_type_of_the_same_name_fails_test() ->
	State = test_connect(),
	{ok,State1} = fly:register_entry(State,{type,{atom,tuple}}),
	{fail, existing_type} = fly:register_entry(State1,{type,{tuple,atom}}),
	%% Can we still read and write the original type?
%%{write,1000} = write(State3, Atom,{"Ewan","Silver",100, male},1000),	
%%	{read,1,{"Ewan","Silver",100,male}} = read(State3, Atom, {"Ewan","","",""}, 1000),
%%	{take_many,1,[_]} = take_many(State3,Atom, {"","",100,""},10),
	fly:close(State1).

big_test() ->
	State = test_connect(),
%% The fact we have to call type_structrure_preamble is a hack at the moment. Need to find someway
%% to hide this inside the returned State reference or something so that we can start to actually
%% return the right kind of tuples.
	Person = person,
	{ok,State1} = fly:register_entry(State,Person,[{string,"name"},{string,"surname"},{int,"age"}, {atom,"sex"}]),

	{write,1000} = fly:write(State1,Person,{"Monty","Silver",2, male},1000),
	{write,1001} = fly:write(State1,Person,{"Elliot","Silver",6, male},1001),

	{read_many,2,[_,_]} = fly:read_many(State1,Person, {"","Silver","",""},10),	

	{read,1,{"Monty","Silver",2,male}} = fly:read(State1,Person, {"Monty","","",""}, 1000),
	{read,1,{"Elliot","Silver",6,male}} = fly:read(State1,Person, {"","",6,""}, 1000),

	{take,1,{"Monty","Silver",2,male}} = fly:take(State1, Person, {"Monty","",""}, 1000),
	{take,1,{"Elliot","Silver",6,male}} = fly:take(State1, Person, {"","",6,""}, 1000),

%% Check that the space is now empty again. Use all available checking calls.	
	{take,0,{}} = fly:take(State1,Person,{"","Silver","",""}, 1),
	{read,0,{}} = fly:read(State1,Person,{"","Silver","",""}, 10),
	{read_many,0,[]} = fly:read_many(State1,Person, {"","Silver","",""},10),	
	
%% Finally we want to check that we can take multiple entries in one go.
	{write,1000} = fly:write(State1, Person,{"Monty","Silver",2,""},1000),
	{write,1001} = fly:write(State1, Person,{"Elliot","Silver",6,""},1001),
	{read_many,2,[_,_]} = fly:read_many(State1, Person, {"","Silver","",""},10),	
	{take_many,2,[_,_]} = fly:take_many(State1, Person, {"","Silver","",""},10),	
	{take,0,{}} = fly:take(State1, Person,{"","Silver","",""}, 1),
	{read,0,{}} = fly:read(State1, Person,{"","Silver","",""}, 10),
	{read_many,0,[]} = fly:read_many(State1, Person, {"","Silver","",""},10),
	{take_many,0,[]} = fly:take_many(State1, Person, {"","Silver","",""},10),	
	
%%	Now_multi  = erlang:now(),
%%	{read_many,0,[]} = read_many(State2,Atom, {"","",100,""},Pause),	
%%	true = Pause*1000 < timer:now_diff(erlang:now(),Now_multi),

	ok = fly:close(State1).

atom_test() ->
	State = test_connect(),
%% The fact we have to call type_structrure_preamble is a hack at the moment. Need to find someway
%% to hide this inside the returned State reference or something so that we can start to actually
%% return the right kind of tuples.
	Person = atom_person,
	{ok,State1} = fly:register_entry(State,Person,[{string,"name"},{atom,"sex"}]),
	{write,1000} = fly:write(State1,Person,{"Monty", male},1000),
	{read,1,{"Monty",male}} = fly:read(State1,Person, {"Monty",""}, 1000),
	fly:close(State1).

contains(Elem,List) ->
	case (length(List) - length(lists:delete(Elem,List))) of
		1 -> true;
		_ -> false
	end.