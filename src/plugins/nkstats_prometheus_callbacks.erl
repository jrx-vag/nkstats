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
-module(nkstats_prometheus_callbacks).
-export([plugin_deps/0]).
-export([nkstats_parse_exporter/2, 
         nkstats_start_exporter/1,
         nkstats_exporter_syntax/0,
         nkstats_register_metric/3,
         nkstats_record_value/3]).

-include("../../include/nkstats.hrl").

%% ===================================================================
%% Plugin callbacks
%%
%% These are used when Nktstats Prometheus is started as a NkSERVICE plugin
%% ===================================================================
plugin_deps() ->
    [nkstats].

%% ===================================================================
%% NkStats callbacks
%% ===================================================================
nkstats_parse_exporter(Data, ParseOpts) ->
    nkstats_prometheus:parse_exporter(Data, ParseOpts).

nkstats_start_exporter(Data) ->
    nkstats_prometheus:start_exporter(Data).

nkstats_exporter_syntax() ->
    nkstats_prometheus:exporter_syntax().

nkstats_register_metric(SrvId, #{class:= prometheus}=Exporter, #{type := Type,
                                                                name := Name,
                                                                description := Desc}) ->
    nkstats_prometheus:register_metric(SrvId, Exporter, Type, Name, Desc);

nkstats_register_metric(_SrvID, #{class:= prometheus}, MetricInfo) ->
    {error, {invalid_metric_info, MetricInfo}};

nkstats_register_metric(_, _, _) ->
    continue.

nkstats_record_value(SrvId, #{class:= prometheus}=Exporter, #{name := Name,
                                                              value := Value}) ->
    nkstats_prometheus:record_value(SrvId, Exporter, Name, Value);

nkstats_record_value(_SrvID, #{class:= prometheus}, MetricValue) ->
    {error, {invalid_metric_value, MetricValue}};

nkstats_record_value(_, _, _) ->
    continue.
