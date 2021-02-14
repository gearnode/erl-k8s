-module(k8s_resources).

-export([get/3, list/2, create/3, delete/3, delete_collection/2, update/4,
         strategic_merge_patch/4,
         collection_path/2, path/3,
         definition/1]).

-export_type([id/0, definition/0, name/0, resource/0, options/0]).

-type id() :: k8s_model:definition_id().

-type definition() ::
    #{path_name := binary(),
      group => binary(),
      version => binary()}.

-type name() :: binary().
-type resource() :: #{atom() := _}.

-type options() ::
        #{context => k8s_config:context_name(),
          namespace => binary(),
          label_selector =>
            k8s_model:apimachinery_apis_meta_v1_label_selector()}.

-spec get(id(), name(), options()) -> k8s:result(resource()).
get(Id, Name, Options) ->
  Request = #{method => <<"GET">>,
              target => path(Id, Name, Options)},
  send_request(Request, Id, Options).

-spec list(id(), options()) -> k8s:result(resource()).
list(Id, Options) ->
  Request = #{method => <<"GET">>,
              target => collection_path(Id, Options)},
  send_request(Request, Id, Options).

-spec create(id(), resource(), options()) -> k8s:result(resource()).
create(Id, Resource, Options) ->
  Request = #{method => <<"POST">>,
              target => collection_path(Id, Options),
              body => encode_resource(Resource, {ref, k8s, Id})},
  send_request(Request, Id, Options).

-spec delete(id(), name(), options()) -> k8s:result(resource()).
delete(Id, Name, Options) ->
  Request = #{method => <<"DELETE">>,
              target => path(Id, Name, Options)},
  send_request(Request, {ref, k8s, apimachinery_apis_meta_v1_status}, Options).

-spec delete_collection(id(), options()) -> k8s:result(resource()).
delete_collection(Id, Options) ->
  Request = #{method => <<"DELETE">>,
              target => collection_path(Id, Options)},
  send_request(Request, {ref, k8s, apimachinery_apis_meta_v1_status}, Options).

-spec update(id(), name(), resource(), options()) -> k8s:result(resource()).
update(Id, Name, Resource, Options) ->
  Request = #{method => <<"PUT">>,
              target => path(Id, Name, Options),
              body => encode_resource(Resource, {ref, k8s, Id})},
  send_request(Request, Id, Options).

-spec strategic_merge_patch(id(), name(), resource(), options()) ->
        k8s:result(resource()).
strategic_merge_patch(Id, Name, Resource, Options) ->
  ContentType = <<"application/strategic-merge-patch+json">>,
  Request = #{method => <<"PATCH">>,
              target => path(Id, Name, Options),
              header => [{<<"Content-Type">>, ContentType}],
              body => encode_resource(Resource, {ref, k8s, Id})},
  send_request(Request, Id, Options).

-spec send_request(mhttp:request(), id(), options()) -> k8s:result(resource()).
send_request(Request0, Id, Options) ->
  LabelSelector = maps:get(label_selector, Options, #{}),
  Request = set_request_label_selector(Request0, LabelSelector),
  RequestOptions = maps:with([context], Options),
  case k8s_http:send_request(Request, RequestOptions) of
    {ok, Response = #{status := Status}} when Status >= 200, Status < 300 ->
      decode_response_body(Response, {ref, k8s, Id});
    {ok, Response = #{status := Status}} ->
      Definition = {ref, k8s, apimachinery_apis_meta_v1_status},
      case decode_response_body(Response, Definition) of
        {ok, StatusData} ->
          {error, {request_error, Status, StatusData}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_request_label_selector(mhttp:request(), Selector) ->
        mhttp:request() when
    Selector :: k8s_model:apimachinery_apis_meta_v1_label_selector().
set_request_label_selector(Request, Selector) when map_size(Selector) == 0 ->
  Request;
set_request_label_selector(Request, Selector) ->
  SelectorString = k8s_label_selectors:format(Selector),
  Target = mhttp_request:target_uri(Request),
  Query = [{<<"labelSelector">>, SelectorString} | uri:query(Target)],
  Request#{target => Target#{query => Query}}.

-spec encode_resource(resource(), jsv:definition()) -> iodata().
encode_resource(Resource, JSVDefinition) ->
  Options = #{null_member_handling => remove,
              disable_verification => true},
  case jsv:generate(Resource, JSVDefinition, Options) of
    {ok, Value} ->
      json:serialize(Value);
    {error, Reason} ->
      error({invalid_resource, Reason})
  end.

-spec decode_response_body(mhttp:response(), jsv:definition()) ->
        k8s:result(resource()).
decode_response_body(Response, JSVDefinition) ->
  case mhttp_response:body(Response) of
    <<>> ->
      {error, empty_response_body};
    Body ->
      case json:parse(Body) of
        {ok, Value} ->
          ValidationOptions = #{null_member_handling => remove,
                                disable_verification => true},
          case jsv:validate(Value, JSVDefinition, ValidationOptions) of
            {ok, Resource} ->
              {ok, Resource};
            {error, Errors} ->
              {error, {invalid_resource_data, Errors}}
          end;
        {error, Reason} ->
          {error, {invalid_json_data, Reason}}
      end
  end.

-spec collection_path(id(), options()) -> binary().
collection_path(Id, Options) ->
  Def = definition(Id),
  BasePath = case maps:find(group, Def) of
               {ok, Group} ->
                 Version = maps:get(version, Def),
                 ["/apis", Group, Version];
               error ->
                 ["/api", "v1"]
             end,
  PathName = maps:get(path_name, Def),
  Path = case maps:find(namespace, Options) of
           {ok, Namespace} ->
             BasePath ++ ["namespaces", Namespace, PathName];
           error ->
             BasePath ++ [PathName]
         end,
  iolist_to_binary(lists:join($/, Path)).

-spec path(id(), name(), options()) -> binary().
path(Id, Name, Options) ->
  BasePath = collection_path(Id, Options),
  <<BasePath/binary, $/, Name/binary>>.

-spec definition(id()) -> definition().
definition(core_v1_config_map) ->
  #{path_name => <<"configmaps">>};
definition(core_v1_config_map_list) ->
  #{path_name => <<"configmaps">>};
definition(core_v1_namespace) ->
  #{path_name => <<"namespaces">>};
definition(core_v1_namespace_list) ->
  #{path_name => <<"namespaces">>};
definition(core_v1_pod) ->
  #{path_name => <<"pods">>};
definition(core_v1_pod_list) ->
  #{path_name => <<"pods">>};
definition(batch_v1_job) ->
  #{path_name => <<"jobs">>,
    group => <<"batch">>,
    version => <<"v1">>};
definition(batch_v1_job_list) ->
  #{path_name => <<"jobs">>,
    group => <<"batch">>,
    version => <<"v1">>};
definition(Id) ->
  error({unknown_resource, Id}).
