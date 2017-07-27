-module(nkstats_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    case nkstats_config:auto_start() of 
        false -> 
            nkstats_sup:start_link();
        true ->
            nkstats_server:start(),
            nkstats_sup:start_link()
    end.

stop(_) ->
    ok.

