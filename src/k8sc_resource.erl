-module(k8sc_resource).

-export([collection_name/1]).

-export_type([type/0, name/0, resource/0]).

-type type() :: atom().

-type name() :: binary().

-type resource() :: #{}.

-callback jsv_definition() -> jsv:definition().

-spec collection_name(name()) -> name().
collection_name(Name) ->
  <<(string:lowercase(Name))/binary, $s>>.
