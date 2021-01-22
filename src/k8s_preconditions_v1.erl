-module(k8s_preconditions_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([preconditions/0]).

-type preconditions() ::
        #{resourceVersion => binary(),
          uid => binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => preconditions_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"Preconditions">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{resourceVersion => string,
         uid => string}}}.
