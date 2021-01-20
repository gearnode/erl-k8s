-module(k8sc_namespace_v1).

-behaviour(k8sc_resource).

-export([jsv_definition/0]).

-export_type([namespace/0]).

-type namespace() ::
        #{kind := k8sc_resource:name(),
          api_version := binary(),
          metadata := k8sc_object_meta_v1:object_meta()}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         api_version => string,
         metadata => {ref, k8sc, object_meta_v1}},
     required =>
       []}}.
