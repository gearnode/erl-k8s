-module(k8sc_status_details_v1).

-behaviour(k8sc_resource).

-export([definition/0, jsv_definition/0]).

-export_type([status_details/0]).

-type status_details() ::
        #{causes => [k8sc_status_cause_v1:status_cause()],
          group => k8sc_resource:group_name(),
          kind => k8sc_resource:name(),
          name => k8sc_resource:name(),
          'retryAfterSeconds' => integer(),
          uid => string}.

-spec definition() -> k8sc_resource:definition().
definition() ->
  #{type => status_details_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    name => <<"StatusDetails">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{causes => {array, #{element => {ref, status_cause_v1}}},
         group => string,
         kind => string,
         name => string,
         'retryAfterSeconds' => integer,
         uid => string},
     required =>
       []}}.
