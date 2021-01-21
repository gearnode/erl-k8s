-module(k8sc_status_cause_v1).

-behaviour(k8sc_resource).

-export([definition/0, jsv_definition/0]).

-export_type([status_cause/0]).

-type status_cause() ::
        #{field => binary(),
          message => binary(),
          reason => binary()}.

-spec definition() -> k8sc_resource:definition().
definition() ->
  #{type => status_cause_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"StatusCause">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{field => string,
         message => string,
         reason => string}}}.
