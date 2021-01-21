-module(k8s_resource).

-export([encode/2, decode/2]).

-export_type([type/0, name/0, kind/0, group_name/0, group_version/0,
              resource/0, definition/0]).

-type type() :: atom().

-type name() :: binary().
-type kind() :: binary().

-type group_name() :: binary().
-type group_version() :: binary().

-type resource() :: #{}.

-type definition() ::
        #{type => type(),
          group := group_name(),
          version := group_version(),
          kind := kind(),
          module := module(),
          path => binary()}.

-callback definition() -> k8s_resource:definition().
-callback jsv_definition() -> jsv:definition().

-spec encode(type(), resource()) -> iodata().
encode(Type, Resource) ->
  Definition = {ref, k8s, Type},
  Options = #{disable_verification => true,
              invalid_member_handling => remove},
  case jsv:generate(Resource, Definition, Options) of
    {ok, Value} ->
      json:serialize(Value);
    {error, Reason} ->
      error({invalid_resource, Reason, Resource, Type})
  end.

-spec decode(type(), binary()) -> {ok, resource()} | {error, term()}.
decode(Type, Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      Definition = {ref, k8s, Type},
      Options = #{disable_verification => true,
                  invalid_member_handling => remove},
      jsv:validate(Value, Definition, Options);
    {error, Reason} ->
      {error, Reason}
  end.
