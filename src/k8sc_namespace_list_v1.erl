-module(k8sc_namespace_list_v1).

-behaviour(k8sc_resource).

-export([definition/0, jsv_definition/0]).

-export_type([namespace/0]).

-type namespace() ::
        #{kind := k8sc_resource:name(),
          'apiVersion' := binary(),
          metadata := k8sc_object_meta_v1:object_meta(),
          items := k8sc_namespace_v1:namespace()}.

-spec definition() -> k8sc_resource:definition().
definition() ->
  #{type => namespace_list_v1,
    group => <<"io.k8s.api.core">>,
    version => <<"v1">>,
    name => <<"NamespaceList">>,
    path => <<"namespaces">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         'apiVersion' => string,
         metadata => {ref, k8sc, object_meta_v1},
         items => {array, #{element => {ref, k8sc, namespace_v1}}}},
     required =>
       [kind, 'apiVersion']}}.
