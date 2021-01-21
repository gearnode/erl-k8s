-module(k8s_object_meta_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([object_meta/0]).

-type object_meta() ::
        #{name => binary(),
          annotations => #{binary() := binary()},
          labels => #{binary() := binary()},
          resource_version => string()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => object_meta_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"ObjectMeta">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{name => string,
         annotations => {object, #{value => string}},
         labels => {object, #{value => string}},
         resource_version => string}}}.
