-module(nkstats_prometheus_svc).
-export([start_services/1, load_objs/0, objs/0, start/0, stop/0]).
-define(SRV, nkstats_prometheus).


start_services(_) ->
    Spec = make_service_spec(),
    case nkservice:start(?SRV, Spec) of
        {ok, _} ->
            ok;
        {error, already_started} ->
            ok;
        {error, Error} ->
            lager:error("Could not start service: ~p (~p)", [Error, Spec]),
            error(service_start_error)
    end.

load_objs() ->
    ok.

objs() -> [].

start() ->
    Spec = make_service_spec(),
    nkservice:start(?SRV, Spec).

stop() ->
    nkservice:stop(?SRV).

make_service_spec() ->
    #{ callback => ?SRV }.
