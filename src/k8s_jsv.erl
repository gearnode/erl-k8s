-module(k8s_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  lists:foldl(fun (M, Acc) ->
                  #{type := Type} = M:definition(),
                  Acc#{Type => M:jsv_definition()}
              end, #{}, k8s_resource_registry:resource_modules()).
