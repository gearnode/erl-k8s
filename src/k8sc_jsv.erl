-module(k8sc_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{metadata => k8sc_metadata:jsv_definition(),
    namespace => k8sc_namespace:jsv_definition()}.
