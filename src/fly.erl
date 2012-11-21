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
%% Description : Gen Server implementation for a Fly server.
%% Author: Ewan Silver
%% Created : 20 Nov 2012
%%% -------------------------------------------------------------------
-module(fly).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([
		 start/0,
		 stop/0,
		 ping/0,
		 read/2,
		 take/2,
		 write/3,
%%		 take/3,
%%		 take/4,
		 read_many/3,
		 take_many/3,
%%		 register_entry/2,
		 register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {state}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,{stop}).

ping() ->
	gen_server:call(?MODULE, {ping}).

register(Typename,Field_info) ->
	gen_server:call(?MODULE,{register,Typename,Field_info}).
  
read(Typename, Template) ->
	gen_server:call(?MODULE,{read,Typename,Template}).

take(Typename, Template) ->
	gen_server:call(?MODULE,{take,Typename,Template}).

write(Typename, Template, Lease) ->
	gen_server:call(?MODULE,{write,Typename,Template,Lease}).
  
read_many(Typename, Template, Limit) ->
	gen_server:call(?MODULE,{read_many,Typename,Template,Limit}).

take_many(Typename, Template, Limit) ->
	gen_server:call(?MODULE,{take_many,Typename,Template,Limit}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	State = fly_protocol:connect(),
    {ok, #state{state=State}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping}, _From, State) ->
	{ping,Tags} = fly_protocol:ping(State#state.state),
    Reply = Tags,
    {reply, Reply, State};
handle_call({register,Typename,Field_info}, _From, State) when is_tuple(Field_info) ->
	{ok,S} = fly_protocol:register_entry(State#state.state,{Typename,Field_info}),
    Reply = ok,
	{reply, Reply, State#state{state=S}};
handle_call({register,Typename,Field_info}, _From, State) when is_list(Field_info) ->
	{ok,S} = fly_protocol:register_entry(State#state.state,Typename,Field_info),
    Reply = ok,
	{reply, Reply, State#state{state=S}};
handle_call({read,Typename,Template}, _From, State) ->
	Reply = fly_protocol:read(State#state.state,Typename,Template),
    {reply, Reply, State};
handle_call({take,Typename,Template}, _From, State) ->
	Reply = fly_protocol:take(State#state.state,Typename,Template),
    {reply, Reply, State};
handle_call({write,Typename,Template,Lease}, _From, State) ->
	{write,L} = fly_protocol:write(State#state.state,Typename,Template,Lease),
	Reply = {ok,L},
    {reply, Reply, State};
handle_call({read_many,Typename,Template,Limit}, _From, State) ->
	{read_many,Num,Entries} = fly_protocol:read_many(State#state.state,Typename, Template, Limit),
	Reply = {read_many,Num,Entries},
    {reply, Reply, State};
handle_call({take_many,Typename,Template,Limit}, _From, State) ->
	{take_many,Num,Entries} = fly_protocol:take_many(State#state.state,Typename, Template, Limit),
	Reply = {take_many,Num,Entries},
    {reply, Reply, State};
handle_call({stop}, _From, State) ->
	Reply = fly_protocol:close(State#state.state),
	{stop, normal, shutdown_ok, State}.



  
  
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

