-module(k8sc_namespace_status_v1).

-behaviour(k8sc_resource).

-export([jsv_definition/0]).

-export_type([namespace_status/0]).

-type namespace_status() ::
        #{phase := binary()}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{phase => string},
     required =>
       []}}.
