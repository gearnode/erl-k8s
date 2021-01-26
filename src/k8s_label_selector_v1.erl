-module(k8s_label_selector_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([label_selector/0]).

-type label_selector() ::
        #{matchExpressions =>
            [k8s_label_selector_requirement_v1:label_selector_requirement()],
          matchLabels => #{binary() := binary()}}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => label_selector_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"LabelSelector">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{matchExpressions =>
           {array, #{element => {ref, k8s, label_selector_requirement_v1}}},
         matchLabels =>
           {object, #{value => string}}}}}.
