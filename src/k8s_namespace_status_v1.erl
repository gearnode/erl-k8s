-module(k8s_namespace_status_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([namespace_status/0]).

-type namespace_status() ::
        #{phase := binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => namespace_status_v1,
    group => <<"io.k8s.api.core">>,
    version => <<"v1">>,
    kind => <<"NamespaceStatus">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{phase => string},
     required =>
       []}}.
