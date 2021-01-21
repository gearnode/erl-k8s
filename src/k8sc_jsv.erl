-module(k8sc_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  lists:foldl(fun (M, Acc) ->
                  #{type := Type} = M:definition(),
                  Acc#{Type => M:jsv_definition()}
              end, #{}, k8sc_resource_registry:resource_modules()).
