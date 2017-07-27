-module(nkstats_config).
-export([plugins/0, 
         port/0, 
         auto_start/0, 
         path/0,
         collectors/0]).

plugins() -> 
    get_env(plugins, []).

port() ->
    get_env(port, 9099).

auto_start() ->
    get_env(auto_start, true).

path() ->
    get_env(path, "/metrics").

collectors() ->
    get_env(collectors, []).

get_env(Key, Default) ->
    case application:get_env(nkstats, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.
