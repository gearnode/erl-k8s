-module(k8s_job_condition_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([job_condition/0]).

-type job_condition() ::
        #{lastProbeTime => k8s_time_v1:time(),
          lastTransitionTime => k8s_time_v1:time(),
          message => binary(),
          reason => binary(),
          status => binary(),
          type => binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => job_condition_v1,
    group => <<"batch">>,
    version => <<"v1">>,
    kind => <<"JobCondition">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{lastProbeTime => {ref, k8s, time_v1},
         lastTransitionTime => {ref, k8s, time_v1},
         message => string,
         reason => string,
         status => string,
         type => string}}}.
