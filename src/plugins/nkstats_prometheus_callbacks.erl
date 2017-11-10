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
-export([nkstats_parse_exporter/2, 
         nkstats_exporter_service_spec/1,
         nkstats_register_metric/4,
         nkstats_record_value/4]).

-include("../../include/nkstats.hrl").

nkstats_parse_exporter(#{ class := prometheus }=Data, ParseOpts) ->
    case nklib_syntax:parse(Data, exporter_syntax(), ParseOpts) of
        {ok, #{config := Config}, _} -> 
            nklib_syntax:parse(Config, prometheus_syntax(), ParseOpts);
        {error, Error} ->
            {error, Error}
    end;

nkstats_parse_exporter(_, _) ->
    continue.

nkstats_exporter_service_spec(Exporter) ->
    #{ id => nkstats_prometheus_exporter,
       plugins => [nkservice_rest],
       callback => nkstats_prometheus_exporter_callbacks,
       nkservice_rest => [#{id => nkstats_prometheus_exporter_rest,
                            url => rest_url(Exporter)}],
       debug => []}.

nkstats_register_metric(prometheus, gauge, Name, Desc) ->
    prometheus_gauge:new([{name, Name}, {help, Desc}]);
        
nkstats_register_metric(prometheus, Type, Name, _) ->
    {error, {invalid_metric_type, Type, Name}};

nkstats_register_metric(_, _, _, _) ->
    continue.

nkstats_record_value(prometheus, gauge, Name, Value) ->
    prometheus_gauge:set(Name, Value);

nkstats_record_value(prometheus, Type, Name, _) ->
    {error, {invalid_metric_type, Type, Name}};

nkstats_record_value(_, _, _, _) ->
    continue.

rest_url(#{ listen_ip := Host,
            listen_port := Port,
            listen_path := Path,
            listen_secure := Secure}) ->
    BinPort = nklib_util:to_binary(Port),
    Http1 = case Secure of true -> <<"https">>; false -> <<"http">> end,
    <<Http1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>.

prometheus_syntax() ->
    #{ 
       listen_ip => host,
       listen_port => {integer, 1, 65535},
       listen_path => basepath,
       listen_secure => boolean,
       '__defaults' => #{
         listen_ip => <<"127.0.0.1">>,
         listen_port => 8081,
         listen_path => <<"/metrics">>,
         listen_secure => false
        }
     }.

exporter_syntax() ->
    #{ id => atom,
       class => atom,
       config => map,
       '__mandatory' => [id, class, config]
     }.
