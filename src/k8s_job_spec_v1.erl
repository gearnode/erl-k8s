-module(k8s_job_spec_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([job_spec/0]).

-type job_spec() ::
        #{activeDeadlineSeconds => integer(),
          backoffLimit => integer(),
          completions => integer(),
          manualSelector => boolean(),
          parallelism => integer(),
          selector => k8s_label_selector_v1:label_selector(),
          %%template => k8s_pod_template_spec_v1:pod_template_spec(),
          ttlSecondsAfterFinished => integer()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => job_spec_v1,
    group => <<"batch">>,
    version => <<"v1">>,
    kind => <<"JobSpec">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{activeDeadlineSeconds => integer,
         backoffLimit => integer,
         completions => integer,
         manualSelector => boolean,
         parallelism => integer,
         selector => {ref, k8s, label_selector_v1},
         %%template => {ref, k8s, pod_template_spec_v1},
         ttlSecondsAfterFinished => integer}}}.
