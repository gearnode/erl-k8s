-module(k8sc_namespace_v1).

-behaviour(k8sc_resource).

-export([definition/0, jsv_definition/0]).

-export_type([namespace/0]).

-type namespace() ::
        #{kind := k8sc_resource:name(),
          api_version := binary(),
          metadata := k8sc_object_meta_v1:object_meta()}.

-spec definition() -> k8sc_resource:definition().
definition() ->
  #{type => namespace_v1,
    group => <<"io.k8s.api.core">>,
    version => <<"v1">>,
    name => <<"Namespace">>,
    path => <<"namespaces">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         api_version => string,
         metadata => {ref, k8sc, object_meta_v1},
         status => {ref, k8sc, namespace_status_v1}},
     required =>
       []}}.
