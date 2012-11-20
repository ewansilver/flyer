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

%% Author: ewan
%% Created: 1 May 2010
%% Description: play(rec) to receive a serve. play(serve) to send a serve.

-module(tennis).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([play/1]).

%%
%% API Functions
%%


play(Type) ->
	S = fly:connect(),
	{ok,S1} = fly:register_entry(S,{ball,{atom}}),
	
	case Type of
		rec -> rec(ping,S1,0);
		serve -> serve(ping,S1,0)
	end	.
%%
%% Local Functions
%%

serve(Ball, State,Count) ->
	{write,10000} = fly:write(State, ball,{Ball},10000),
	io:fwrite("Served ~w - #~w~n",[Ball,Count]),
	rec(switch_ball(Ball), State,Count+1).

rec(Ball, State,Count) ->
	{take,1,{Ball}} = fly:take(State, ball,{Ball},{10000,10}),
	io:fwrite("Received ~w - #~w~n",[Ball,Count]),
	serve(switch_ball(Ball), State,Count+1).

switch_ball(Ball) ->
	case Ball of
		ping -> pong;
		pong -> ping
	end.