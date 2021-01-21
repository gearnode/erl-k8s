-module(k8s_status_v1).

-behaviour(k8s_resource).

-export([definition/0, jsv_definition/0]).

-export_type([status/0]).

-type status() ::
        #{kind => k8s_resource:name(),
          apiVersion => binary(),
          code => integer(),
          details => k8s_status_details_v1:status_details(),
          message => binary(),
          metadata => k8s_list_meta_v1:list_meta(),
          reason => binary(),
          status => binary()}.

-spec definition() -> k8s_resource:definition().
definition() ->
  #{type => status_v1,
    group => <<"io.k8s.apimachinery.pkg.apis.meta">>,
    version => <<"v1">>,
    kind => <<"Status">>,
    module => ?MODULE}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         apiVersion => string,
         code => integer,
         details => {ref, status_details_v1},
         message => string,
         metadata => {ref, list_meta_v1},
         reason => string,
         status => string}}}.
