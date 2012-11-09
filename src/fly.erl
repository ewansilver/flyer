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

-module(fly).

%%
%% Include files
%%
-record(state,
			{socket,
	 		type_structures=[]}).
%%
%% Exported Functions
%%
-export([connect/0, connect/2,close/1,
		 ping/1,
		 read/4,
		 write/4,
		 take/4,
		 read_many/4,
		 take_many/4,
		 register_entry/2,
		 register_entry/3]).

%%-export([binary_read/0]).
%%
%% API Functions
%%
-define(ZnInt, 32/big).
-define(ZnLong, 64/big).

connect() ->
	connect("localhost",4396).

connect(Host,Port) ->
    {ok, Sock} = gen_tcp:connect(Host,Port, 
                                 [binary, 
								  {active, false} ]),
	#state{socket=Sock}.

close(State) ->
	gen_tcp:close(State#state.socket).
	
%% Return: {ping,[tagnames]}
ping(State) ->
	Header = <<16#FAB10000:?ZnInt>>,
    ok = gen_tcp:send(State#state.socket, Header),
	{ok,<<_TagCount:?ZnLong>>} = gen_tcp:recv(State#state.socket,8),
	{ok,Response} = gen_tcp:recv(State#state.socket,0),
	{ping, get_tags(Response,[])}.

%% Return: {read,number of matches: 1 or 0,Entry or any empty tuple}
read(State,Type_name, Template, Wait_time) ->
	case lists:keysearch(Type_name, 1,State#state.type_structures) of
		false -> {fail, missing_type};
		{value,{_,Type_channel,_,Field_info}} ->
			Template_entry = create_Zn_entry(Template),
			Header = <<16#FAB10001:?ZnInt>>,
			Binary = <<Header/binary,Type_channel:?ZnInt, Template_entry/binary,Wait_time:?ZnLong>>,
			parse_single_entry_responses(read,State#state.socket,Field_info,Binary,Wait_time)
	end.

%% Return: {take,number of matches: 1 or 0,Entry or any empty tuple}
take(State,Type_name, Template, Wait_time) ->
	case lists:keysearch(Type_name, 1,State#state.type_structures) of
		false -> {fail, missing_type};
		{value,{_,Type_channel,_,Field_info}} ->
			Template_entry = create_Zn_entry(Template),
			Header = <<16#FAB10002:?ZnInt>>,
			Binary = <<Header/binary,Type_channel:?ZnInt, Template_entry/binary,Wait_time:?ZnLong>>,
			parse_single_entry_responses(take,State#state.socket,Field_info, Binary, Wait_time)
	end.

parse_single_entry_responses(Type,Sock,Field_info,Request_binary, Wait_time) ->
	Remaining_time = pause(Wait_time, 100),
	ok = gen_tcp:send(Sock, Request_binary),
	{ok,<<Num_fields:?ZnLong>>} = gen_tcp:recv(Sock,8),
	case Num_fields of
		0 -> case Remaining_time of
				0 -> {Type,0, {}};
				_ -> parse_single_entry_responses(Type, Sock, Field_info, Request_binary, Remaining_time)
			 end;
		_ -> {ok,Binary} = gen_tcp:recv(Sock,0),
			{Entry,_} = get_object(Num_fields,Binary,Field_info),
			{Type, 1,Entry}
	end.

pause(Remaining_time,Wait) when Remaining_time < Wait ->
	timer:sleep(Remaining_time),
	0;
pause(Remaining_time,Wait) ->
	timer:sleep(Wait),
	(Remaining_time-Wait).

%% Return: {write,Lease time in ms}
write(State,Type_name, Template, Lease) ->
	case lists:keysearch(Type_name, 1,State#state.type_structures) of
		false -> {fail, missing_type};
		{value,{_,Type_channel,_,_}} ->
			Template_entry = create_Zn_entry(Template),
			Header = <<16#FAB10003:?ZnInt>>,
		    ok = gen_tcp:send(State#state.socket, <<Header/binary,Type_channel:?ZnInt, Template_entry/binary,Lease:?ZnLong>>),
			{ok,<<Lease_granted:?ZnLong>>} = gen_tcp:recv(State#state.socket,8),
			{write,Lease_granted}
	end.

%% Return: {read_many,number of matches,list of entries - could be empty}
read_many(State,Type_name, Template, Limit) ->
	case lists:keysearch(Type_name, 1,State#state.type_structures) of
		false -> {fail, missing_type};
		{value,{_,Type_channel,_,Field_info}} ->
			Template_entry = create_Zn_entry(Template),
			Header = <<16#FAB10006:?ZnInt>>,
%% Question: what is ignore for?
			Ignore = 0,
		    ok = gen_tcp:send(State#state.socket,
						<<Header/binary,Type_channel:?ZnInt, Template_entry/binary,Limit:?ZnLong,Ignore:?ZnLong>>),
			parse_multiple_entry_responses(read_many,State#state.socket,Field_info)
	end.

%% Return: {take_many,number of matches,list of entries - could be empty}
take_many(State,Type_name, Template, Limit) ->
	case lists:keysearch(Type_name, 1,State#state.type_structures) of
		false -> {fail, missing_type};
		{value,{_,Type_channel,_,Field_info}} ->
			Template_entry = create_Zn_entry(Template),
			Header = <<16#FAB10007:?ZnInt>>,
		    ok = gen_tcp:send(State#state.socket, 
						<<Header/binary,Type_channel:?ZnInt, Template_entry/binary,Limit:?ZnLong>>),
			parse_multiple_entry_responses(take_many,State#state.socket,Field_info)
	end.

parse_multiple_entry_responses(Type,Sock,FieldTypes) ->
	{ok,<<Num_entries:?ZnLong>>} = gen_tcp:recv(Sock,8),
	case Num_entries of
		0 -> {Type,0, []};
		_ -> {ok,Binary} = gen_tcp:recv(Sock,0),
			{Entries,_} = get_entries(Num_entries,Binary,[],FieldTypes),
			{Type, Num_entries,Entries}
	end.

register_entry(State,{Typename,FieldInfo}) ->
	register_entry(State,Typename,[{Field,"name"} || Field <- tuple_to_list(FieldInfo)]).

register_entry(State,Typename,Field_info) ->
	FieldInfo = [{atom_to_list(Field),Name} || {Field,Name} <- Field_info],
	Entry_layout = create_entry_layout(Typename, FieldInfo),
	Header = <<16#FAB1000A:?ZnInt>>,
    ok = gen_tcp:send(State#state.socket, <<Header/binary,Entry_layout/binary>>),
	{ok,<<Reply_code:?ZnLong>>} = gen_tcp:recv(State#state.socket,8),
	case Reply_code of
		0 ->
			{ok,Entry_layout_rtn} = gen_tcp:recv(State#state.socket,0),
			{_,TypeChannel,_,_} = decode_entry_layout(Entry_layout_rtn),
			{ok,State#state{type_structures = [{Typename,TypeChannel,null,FieldInfo}|State#state.type_structures]}};
		_ -> {fail, existing_type}
	end.

%%
%% Local Functions
%%

-spec create_Zn_string(atom()) -> binary().
create_Zn_string(String) ->
	Binary = term_to_binary(String),
	Length = size(Binary),
	<<Length:?ZnInt,Binary/binary>>.

%% FieldInfo [{fieldType,fieldName}]
create_entry_layout(Typename,FieldInfo) ->
	Typename_bin = create_Zn_string(Typename),
	FI_Length = length(FieldInfo),
	FI_fun = fun({FieldType,FieldName},Binary) -> TypeString = create_Zn_string(FieldType),
												  NameString = create_Zn_string(FieldName),
					 								<<Binary/binary,TypeString/binary,NameString/binary>> end,
	FI_binaries = lists:foldl(FI_fun,<<FI_Length:?ZnInt>>,FieldInfo),
	<<Typename_bin/binary,0:?ZnInt,FI_binaries/binary>>.

decode_entry_layout(<<TypeNameSize:?ZnInt,TypeName:TypeNameSize/binary,TypeChannel:?ZnInt,FieldInfoCount:?ZnInt,Fields/binary>>) ->
	Field_list = [{FieldType,FieldName} ||<<TypeSize:?ZnInt,FieldType:TypeSize/binary,NameSize:?ZnInt,FieldName:NameSize/binary>> <= Fields],
	{binary_to_list(TypeName),TypeChannel,FieldInfoCount,Field_list}.

get_tags(<<>>,List) ->
	lists:reverse(List);
get_tags(Binary,List) ->
	{_, String,Remainder}= get_Zn_string(Binary),
	get_tags(Remainder,[String|List]).

get_Zn_string(<<Size:?ZnInt,String:Size/binary,Remainder/binary>>) ->
	{Size,binary_to_list(String),Remainder}.

encode_Zn_object(Object) when is_binary(Object) -> 
	create_binary_object(Object);
encode_Zn_object(Object) when is_tuple(Object) ->
	create_binary_object(term_to_binary(Object));
encode_Zn_object(Object) when is_list(Object) ->
	create_binary_object(list_to_binary(Object));
encode_Zn_object(Object) when is_integer(Object) ->
		create_binary_object(term_to_binary(Object));
encode_Zn_object(Object) when is_atom(Object) ->
		create_binary_object(term_to_binary(Object)).

%%TODO: this is misnamed
create_binary_object(Binary) ->
	Length = size(Binary),
	<<Length:?ZnLong,Binary/binary>>.

decode_Zn_object(<<Size:?ZnInt,Object:Size/binary,Remainder/binary>>) ->
	{Size,Object,Remainder}.

%% Objects = [Object]
create_Zn_entry(Objects) when is_list(Objects) ->
	Length = length(Objects),
	lists:foldl(fun(Object,Binary) -> <<Binary/binary,Object/binary>> end, <<Length:?ZnInt>>,Objects);
create_Zn_entry(Tuple) ->
	create_Zn_entry(lists:map(fun(Element) -> encode_Zn_object(Element) end, tuple_to_list(Tuple))).

get_Zn_entry(<<Num_of_fields:?ZnLong,Binary/binary>>,FieldTypes) ->
	{Fields,RemainingBinary} = get_object(Num_of_fields,Binary,FieldTypes),
	{Fields,RemainingBinary}.

get_entries(0,Remaining_binary,List,_) -> 
	{lists:reverse(List),Remaining_binary};
get_entries(Num_entries,Entries,List,FieldTypes) ->
	{Entry,RemainingBinary} = get_Zn_entry(Entries,FieldTypes),
	get_entries((Num_entries-1), RemainingBinary,[Entry|List],FieldTypes).

get_object(Num_fields,Binary,FieldTypes) -> 
	%% We have to double the number of fields becuase Fly core is expecting a field name in the first
	%% (and every other) location. To turn this into an Erlang tuple we then need to strip out those empty tuples.
	{Fields,Remainder} = get_objects((Num_fields*2), Binary,[]),
	{list_to_tuple(remove_field_names(Fields,[],FieldTypes)),Remainder}.

remove_field_names([],Acc,_) -> 
	lists:reverse(Acc);
remove_field_names([_Name,Type|T],Acc,[F|FieldTypes]) -> 
	remove_field_names(T,[decode_field_type(Type,F)|Acc],FieldTypes).

%% Todo: Decode field type is the place we will work out how to deserialise things into something other than strings in the future.
%% We should be able to return atoms and terms to!
%% TODO: not sure why the binary is needed here. It seems to die on read_many if I don't catch an empty binary here.
decode_field_type(<<>>,_) -> 
	<<>>;
decode_field_type(Field,{Type,_}) -> 
	case Type of
		"atom" -> binary_to_term(Field);
		"int" -> binary_to_term(Field);
		"tuple" -> binary_to_term(Field);
		"binary" -> Field;
		"string" -> binary_to_list(Field);
		_ 	-> binary_to_list(Field)
	end.

get_objects(0,Remainder,List) ->
	{lists:reverse(List),Remainder};
get_objects(Num_of_fields, Objects,List) ->
	{_Size, Object,Remainder} = decode_Zn_object(Objects),
	get_objects((Num_of_fields-1), Remainder,[Object|List]).
