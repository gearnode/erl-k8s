-module(k8s_namespace_list_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([namespace/0]).

-type namespace() ::
        #{kind => k8s_resource:kind(),
          apiVersion => binary(),
          metadata := k8s_object_meta_v1:object_meta(),
          items := k8s_namespace_v1:namespace()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => namespace_list_v1,
    group => <<"io.k8s.api.core">>,
    version => <<"v1">>,
    kind => <<"NamespaceList">>,
    path => <<"namespaces">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         apiVersion => string,
         metadata => {ref, k8s, object_meta_v1},
         items => {array, #{element => {ref, k8s, namespace_v1}}}}}}.
