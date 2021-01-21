-module(k8s_list_meta_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([list_meta/0]).

-type list_meta() ::
        #{continue => binary(),
          remainingItemCount => integer(),
          resourceVersion => binary(),
          selfLink => binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => list_meta_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"ListMeta">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{continue => string,
         remainingItemCount => integer,
         resourceVersion => string,
         selfLink => string}}}.
