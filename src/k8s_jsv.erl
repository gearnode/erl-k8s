-module(k8s_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{}.
