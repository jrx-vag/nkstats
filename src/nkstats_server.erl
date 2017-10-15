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
-module(nkstats_server).
-export([start/0, do/1]).
-define(APP_NAME, nkstats).
-define(SERVER_NAME, "NkStats Server").
-include_lib("inets/include/httpd.hrl").

start() ->
    nkstats_prometheus:setup(),
    inets:start(httpd, [
                        {modules, [
                                  ?MODULE 
                                  ]},
                        {port, nkstats_config:port()},
                        {server_name, ?SERVER_NAME},
                        {document_root, code:priv_dir(?APP_NAME)},
                        {server_root, code:priv_dir(?APP_NAME)}
                       ]).


do(Req) ->
    Headers = fun(Name, Default) ->
                      proplists:get_value(Name, Req#mod.parsed_header, Default)
              end,

    case nkstats_config:path() =:= Req#mod.request_uri of
        true ->
            {Code, Headers2, Body} = nkstats_prometheus:metrics(Headers),
            reply(Code, Headers2, Body);
        false ->
            reply(404, [], <<>>)
    end.


reply(Code, Headers, Body) ->
    Length = integer_to_list(iolist_size(Body)),
    Headers2 = Headers++ [{code, Code}, 
                          {content_length, Length},
                          {content_type, "text/plain"}],
    {break, [{response, {response, Headers2, [Body]}}]}.

