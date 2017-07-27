-module(nkstats_config).
-export([collectors/0, 
         auto_start/0,
         port/0,
         path/0]).

collectors() ->
    get_env(collectors, []).

auto_start() ->
    get_env(auto_start, true).

port() -> 
    get_env(port, 9399).


path() -> 
    get_env(path, "/metrics").




get_env(Key, Default) ->
    case application:get_env(nkstats, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.
