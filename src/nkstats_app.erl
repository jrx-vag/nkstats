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
-export([get_exporter/1, put_exporter/2, get/2]).
-include("nkstats.hrl").

start(_, _) ->
    Syntax = #{exporters=> {list, map}},
    case nklib_config:load_env(?APP, Syntax) of
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

get_exporter(Id) ->
    get({exporter, nklib_util:to_binary(Id)}, not_found).

put_exporter(Id, exporter) ->
    Ids = get(exporter_ids, []),
    put(exporter_ids, nklib_util:store_value(nklib_util:to_binary(Id), Ids)),
    put({exporter, Id}, exporter).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).
