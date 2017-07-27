-module(nkstats_prometheus).
-export([setup/0, metrics/1]).

setup() ->
    [ setup(C) || C <- nkstats_config:collectors() ].

metrics(_) ->
    collect(),
    Scrape = prometheus_text_format:format(),
    ContentType = prometheus_text_format:content_type(),
    {200, [{content_type, binary_to_list(ContentType)}], Scrape}. 
    
setup(C) ->
    [ setup_metric(M) || M <- C:metrics(specs) ].
  
setup_metric({Type, Name, Labels, Help}) ->
    setup_metric(Type, Name, Labels, Help );

setup_metric({Type, Name, Help}) ->
    setup_metric(Type, Name, Help).

setup_metric(gauge, Name, Help) ->
    prometheus_gauge:new([{name, Name}, {help, Help}]).

setup_metric(gauge, Name, Labels, Help) ->
    prometheus_gauge:new([{name, Name}, {labels, Labels}, {help, Help}]).

collect() ->
    [ collect(C) || C <- nkstats_config:collectors() ].

collect(C) ->
    [ collect_metric(M) || M <- C:metrics(values) ].

collect_metric({Type, Name, Labels, Help}) ->
    collect_metric(Type, Name, Labels, Help );

collect_metric({Type, Name, Help}) ->
    collect_metric(Type, Name, Help).

collect_metric(gauge, Name, Value) ->
    prometheus_gauge:set(Name, Value).

collect_metric(gauge, Name, Labels, Value) ->
    prometheus_gauge:set(Name, Labels, Value).

