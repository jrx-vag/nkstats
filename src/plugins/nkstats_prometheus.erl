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
-module(nkstats_prometheus).
-export([parse_exporter/2,
         start_exporter/1,
         exporter_syntax/0,
         register_metric/5,
         record_value/4]).
-include("../../include/nkstats.hrl").

parse_exporter(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=> atom}) of
        {ok, #{class:=prometheus}, _} ->
            case nklib_syntax:parse(Data, exporter_syntax(), ParseOpts) of
                {ok, Exporter, UnknownFields} ->
                    {ok, Exporter, UnknownFields};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.

exporter_syntax() ->
    Base = nkstats_util:exporter_syntax(),
    Base#{
        config := #{
            listen_ip => binary,
            listen_port => integer,
            listen_path => binary,
            '__mandatory' => [listen_ip, listen_port, listen_path]
        }
    }.

-spec start_exporter(nkstats:exporter()) ->
    ok | {error, term()}.

start_exporter(#{ config := #{
                             listen_ip := ListenIp,
                             listen_port := ListenPort,
                             listen_path := ListenPath
                   }}) ->

    Opts = #{tcp_listeners=>1, 
             idle_timeout=>60000, 
             path => ListenPath },
    
    case nkpacket:start_listener({?MODULE, http, nklib_util:to_ip(ListenIp), ListenPort}, Opts) of
        {ok, _} -> ok;
        {error, Error} -> {error, Error}
    end.


-spec register_metric(nkservice:id(),
             nkstats:exporter(),
             nkstats:metric_type(),
             nkstats:metric_name(),
             nkstats:metric_description()) ->
    ok | {error, term()}.

register_metric(_SrvId, #{ config := #{
                             listen_ip := _ListenIp,
                             listen_port := _ListenPort,
                             listen_path := _ListenPath
                            }}, _Type, _Name, _Desc) ->
    ok.

-spec record_value(nkservice:id(),
             nkstats:exporter(),
             nkstats:metric_name(),
             nkstats:metric_value()) ->
    ok | {error, term()}.

record_value(_SrvId, #{ config:=#{ 
                             listen_ip := _ListenIp,
                             listen_port := _ListenPort,
                             listen_path := _ListenPath
                            }}, _Name, _Value) ->

    ok.
