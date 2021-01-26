-module(k8s_time_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([time/0]).

-type time() :: calendar:datetime().

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => time_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"Time">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  datetime.
