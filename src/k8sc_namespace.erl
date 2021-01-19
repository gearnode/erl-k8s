-module(k8sc_namespace).

-export([jsv_definition/0]).

-export_type([namespace/0]).

-type namespace() ::
        #{kind := binary(),
          api_version := binary(),
          metadata := k8sc_metadata:metadata()}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{kind => string,
         api_version => string,
         metadata => {ref, k8sc, metadata}},
     required =>
       []}}.
