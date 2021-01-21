-module(k8sc_resources).

-export([get/2]).

-type request_options() ::
        #{namespace => binary(),
          context => k8sc_config:context_name()}.

%% TODO verbs: create, delete, deletecollection, get, list, patch, update, watch

-spec get(k8sc_resource:type(), k8sc_resource:name()) ->
        {ok, k8sc_resource:resource()} | {error, term()}.
get(Type, Name) ->
  get(Type, Name, #{}).

-spec get(k8sc_resource:type(), k8sc_resource:name(), request_options()) ->
        {ok, k8sc_resource:resource()} | {error, term()}.
get(Type, Name, Options) ->
  ResourceDef = k8sc_resource_registry:resource_def(Type),
  Namespace = maps:get(namespace, Options, undefined),
  BasePath = resource_path(ResourceDef, Namespace),
  Path = iolist_to_binary([BasePath, $/, Name]),
  Request = #{method => <<"GET">>, target => Path},
  SendRequestOptions = maps:with([context], Options),
  case k8sc_http:send_request(Request, SendRequestOptions) of
    {ok, Response} ->
      Body = mhttp_response:body(Response),
      k8sc_resource:decode(Type, Body);
    {error, Reason} ->
      {error, Reason}
  end.

-spec resource_path(k8sc_resource_registry:resource_def(),
                   Namespace :: binary() | undefined) -> iolist().
resource_path(Def = #{name := Name}, Namespace) ->
  Collection = k8sc_resource:collection_name(Name),
  case Namespace of
    undefined ->
      [base_resource_path(Def), $/, Collection];
    _ ->
      [base_resource_path(Def), "/namespaces/", Namespace, $/, Collection]
  end.

-spec base_resource_path(k8sc_resource_registry:resource_def()) -> iolist().
base_resource_path(Def = #{version := Version}) ->
  case maps:get(group, Def) of
    <<"io.k8s.api.core">> ->
      ["/api/", Version];
    Group ->
      ["/apis/", Group, $/, Version]
  end.
