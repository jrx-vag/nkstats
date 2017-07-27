-module(nkstats_config).
-export([collectors/0]).

collectors() ->
    get_env(collectors, []).

get_env(Key, Default) ->
    case application:get_env(nkstats, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.
