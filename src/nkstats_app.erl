%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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
-module(nkstats_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([all_exporters/0]).
-include("nkstats.hrl").

start(_, _) ->
    case all_exporters() of
        {ok, _} ->
            {ok, Vsn} = application:get_key(?APP, vsn),
            ?INFO("v~s is starting", [Vsn]),
            {ok, Pid} = nkstats_sup:start_link(),
            {ok, Pid};
        {error, Error} ->
            error({syntax_error, Error})
    end.

stop(_) ->
    ok.

all_exporters() -> 
    Syntax = exporters_syntax(),
    nklib_config:load_env(?APP, Syntax).
        

exporters_syntax() ->
    #{ exporters => {list, map},
       '__defaults' => #{
         exporters => []
        }
     }.
