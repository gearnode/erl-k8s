-module(k8s_delete_options_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([delete_options/0]).

-type delete_options() ::
        #{apiVersion => binary(),
          dryRun => [binary()],
          gracePeriodSeconds => integer(),
          kind => binary(),
          orphanDependents := boolean(),
          preconditions := k8s_preconditions_v1:preconditions(),
          propagationPolicy := binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => delete_options_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"DeleteOptions">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{apiVersion => string,
         dryRun => [string],
         gracePeriodSeconds => integer,
         kind => string,
         orphanDependents => boolean,
         preconditions => {ref, preconditions_v1},
         propagationPolicy => string}}}.
