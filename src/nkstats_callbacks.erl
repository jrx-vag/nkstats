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
-export([nkstats_get_exporter/2,
        nkstats_parse_exporter/2,
        nkstats_register_metric/3,
        nkstats_record_value/3]).
-include("nkstats.hrl").

%% ===================================================================
%% Plugin callbacks
%%
%% These are used when nkstats is started as a NkSERVICE plugin
%% ===================================================================

plugin_deps() -> [].

plugin_syntax() -> #{}.

service_init(_Service,  #{id:=SrvId}=State) ->
    ?INFO("service init: ~p", [SrvId]),
    lists:foreach(
        fun
            (#{id := Id}=Data) ->
                case SrvId:nkstats_parse_exporter(Data, #{}) of
                    {ok, Exporter, _} ->
                        ?WARN("loading exporter ~p", [Id]),
                        case SrvId:nkstats_start_exporter(Exporter) of
                            ok -> 
                                nkstats_app:put_exporter(nklib_util:to_binary(Id), Exporter);
                            {error, Error} ->
                                ?WARN("error with exporter ~p: ~p", [Data, Error])
                        end;
                    {error, Error} ->
                        ?WARN("error with exporter ~p: ~p", [Data, Error])
                end;
            (Data) ->
                ?WARN("invalid exporter: ~p", [Data])
        end,
        nkstats_app:get(exporters, [])),
    {ok, State}.


%% ===================================================================
%% Stats processing callbacks
%% ===================================================================

-spec nkstats_get_exporter(nkservice:id(), nkstats:exporter_id()) ->
    {ok, nkstats:exporter()} | {error, term()}.

nkstats_get_exporter(_SrvId, Id) ->
    case nkstats_app:get_exporter(Id) of
        not_found ->
            {error, {exporter_not_found, Id}};
        Exporter -> 
            {ok, Exporter}
    end.

-spec nkstats_parse_exporter(map(), nklib_syntax:parse_opts()) ->
    {ok, nkstats:exporter(), [binary()]} | {error, term()}.

nkstats_parse_exporter(_Exporter, _Opts) ->
    {error, invalid_exporter}.

%% @doc Register information about a metric
-spec nkstats_register_metric(nkservice:id(), 
                              nkstats:exporter(),
                              nkstats:metric_info()) ->
    ok | {error, term()}.

nkstats_register_metric(_SrvId, _Exporter, _MetricInfo) ->
    {error, invalid_exporter}.

%% @doc Record a value for a metric
-spec nkstats_record_value(nkservice:id(), 
                              nkstats:exporter(),
                              nkstats:metric_value()) ->
    ok | {error, term()}.

nkstats_record_value(_SrvId, _Exporter, _MetricValue) ->
    {error, invalid_exporter}.

