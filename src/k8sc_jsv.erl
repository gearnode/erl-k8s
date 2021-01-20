-module(k8sc_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  maps:fold(fun (Type, #{module := Module}, Acc) ->
                Acc#{Type => Module:jsv_definition()}
            end, #{}, k8sc_resource_registry:resource_definitions()).
