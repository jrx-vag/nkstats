-module(nkstats_prometheus).
-export([setup/0, metrics/1]).

setup() ->
    [ setup(C) || C <- nkstats_config:collectors() ].
    
setup(C) ->
    [ setup_metric(M) || M <- C:metrics(specs) ].
   
setup_metric(#{ type := Type, name := Name, help := Help}) ->
    setup_metric(Type, Name, Help ).

setup_metric(gauge, Name, Help) ->
    prometheus_gauge:new([{name, Name}, {help, Help}]).


collect() ->
    [ collect(C) || C <- nkstats_config:collectors() ].

collect(C) ->
    [ collect_metric(M) || M <- C:metrics(values) ].

collect_metric(#{ type := Type, name := Name, value := Value}) ->
    collect_metric(Type, Name, Value).

collect_metric(gauge, Name, Value) ->
    prometheus_gauge:set(Name, Value).

metrics(_) ->
    collect(),
    Scrape = prometheus_text_format:format(),
    ContentType = prometheus_text_format:content_type(),
    {200, [{content_type, binary_to_list(ContentType)}], Scrape}. 
