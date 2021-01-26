-module(k8s_label_selector_requirement_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([label_selector_requirement/0]).

-type label_selector_requirement() ::
        #{key := binary(),
          operator := binary(),
          values => [binary()]}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => label_selector_requirement_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"LabelSelectorRequirement">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{key => string,
         operator => string,
         values => {array, #{element => string}}},
     required => [key, operator]}}.
