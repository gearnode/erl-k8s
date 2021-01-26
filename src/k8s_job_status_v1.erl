-module(k8s_job_status_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([job_status/0]).

-type job_status() ::
        #{active => integer(),
          completionTime => k8s_time_v1:time(),
          conditions => [k8s_job_condition_v1:job_condition()],
          failed => integer(),
          startTime => k8s_time_v1:time(),
          succeeded => integer()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => job_status_v1,
    group => <<"batch">>,
    version => <<"v1">>,
    kind => <<"JobStatus">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{active => integer,
         completionTime => {ref, k8s, time_v1},
         conditions => {array, #{element => {ref, k8s, job_condition_v1}}},
         failed => integer,
         startTime => {ref, k8s, time_v1},
         succeeded => integer}}}.
