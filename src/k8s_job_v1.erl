-module(k8s_job_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([job/0]).

-type job() ::
        #{kind => k8s_resource:kind(),
          apiVersion => binary(),
          metadata => k8s_object_meta_v1:object_meta()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => job_v1,
    group => <<"batch">>,
    version => <<"v1">>,
    kind => <<"Job">>,
    path => <<"jobs">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         apiVersion => string,
         metadata => {ref, k8s, object_meta_v1}}}}.
