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
-module(nkstats_callbacks).
-export([plugin_deps/0, plugin_syntax/0]).
-export([service_init/2]).
-export([nkstats_parse_exporter/2,
         nkstats_exporter_service_spec/1,
         nkstats_register_metric/3,
         nkstats_record_value/3]).
-include("nkstats.hrl").

plugin_deps() -> [].

plugin_syntax() -> #{}.

service_init(_Service,  #{id:=SrvId}=State) ->
    ?INFO("service init: ~p, with state: ~p and pid: ~p", [SrvId, State, self()]),
    case nkstats_app:all_exporters() of 
        {ok, #{ exporters :=  Configs}} ->
            lists:foreach(fun(Config) ->
                                  case nkstats:parse_exporter(SrvId, Config, #{}) of 
                                      {ok, Exporter, _} -> 
                                          Spec = nkstats:exporter_service_spec(SrvId, Exporter), 
                                          start_exporter(Spec);
                                      {error, Error} ->
                                          ?WARN("cannot start exporter: ~p", [Error])
                                  end
                          end, Configs);
        {error, Error} ->
            ?WARN("invalid configuration: ~p", [Error])
    end,
    {ok, State}.


start_exporter(#{id := SrvId}=Spec) ->
    case nkservice:start(SrvId, Spec) of
        {ok, _} ->
            lager:info("Started exporter ~p", [SrvId]),
            ok;
        {error, already_started} ->
            ok;
        {error, Error} ->
            lager:warning("Error starting exporter ~p: ~p", [Spec, Error]),
            error(service_start_error)
    end.

nkstats_parse_exporter(_Exporter, _Opts) ->
    {error, invalid_exporter}.

nkstats_exporter_service_spec(_Exporter) ->
    {error, invalid_exporter}.

nkstats_register_metric(_SrvId, _Exporter, _MetricInfo) ->
    {error, invalid_exporter}.

nkstats_record_value(_SrvId, _Exporter, _MetricValue) ->
    {error, invalid_exporter}.

