%% Copyright (c) 2020-2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(k8s_resources).

-export([get/3, list/2, create/3, delete/3, delete_collection/2, update/4,
         strategic_merge_patch/4,
         decode_response_body/2,
         collection_path/2, path/3,
         definition/1,
         add_options_query/3]).

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
          query => uri:query(),
          label_selector =>
            k8s_model:apimachinery_apis_meta_v1_label_selector(),
          field_manager => binary()}.

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
  send_request(Request, delete_response_id(Id), Options).

-spec delete_collection(id(), options()) -> k8s:result(resource()).
delete_collection(Id, Options) ->
  Request = #{method => <<"DELETE">>,
              target => collection_path(Id, Options)},
  send_request(Request, apimachinery_apis_meta_v1_status, Options).

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

-spec send_request(mhttp:request(), id(), options()) ->
        k8s:result(resource()).
send_request(Request0, Id, Options) ->
  Request = lists:foldl(fun (F, Req) -> F(Req, Options) end,
                        Request0, [fun set_request_query/2,
                                   fun set_request_label_selector/2,
                                   fun set_request_field_manager/2]),
  RequestOptions = maps:with([context], Options),
  case k8s_http:send_request(Request, RequestOptions) of
    {ok, Response = #{status := Status}} when Status >= 200, Status < 300 ->
      Definition = {ref, k8s, Id},
      decode_response_body(Response, Definition);
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

-spec set_request_query(mhttp:request(), options()) -> mhttp:request().
set_request_query(Request, #{query := Query}) ->
  Target = mhttp_request:target_uri(Request),
  Request#{target => Target#{query => uri:query(Target) ++ Query}};
set_request_query(Request, _) ->
  Request.

-spec set_request_label_selector(mhttp:request(), options()) ->
        mhttp:request().
set_request_label_selector(Request, #{label_selector := Selector}) when
    map_size(Selector) == 0 ->
  Request;
set_request_label_selector(Request, #{label_selector := Selector}) ->
  SelectorString = k8s_label_selectors:format(Selector),
  Target = mhttp_request:target_uri(Request),
  Query = [{<<"labelSelector">>, SelectorString} | uri:query(Target)],
  Request#{target => Target#{query => Query}};
set_request_label_selector(Request, _) ->
  Request.

-spec set_request_field_manager(mhttp:request(), options()) ->
        mhttp:request().
set_request_field_manager(Request, #{field_manager := FieldManager}) ->
  Target = mhttp_request:target_uri(Request),
  Query = [{<<"fieldManager">>, FieldManager} | uri:query(Target)],
  Request#{target => Target#{query => Query}};
set_request_field_manager(Request, Options) ->
  K8sOptions = persistent_term:get(k8s_options),
  case maps:find(field_manager, K8sOptions) of
    {ok, FieldManager} ->
      set_request_field_manager(Request,
                                Options#{field_manager => FieldManager});
    error ->
      Request
  end.

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
      Status = mhttp_response:status(Response),
      case json:parse(Body) of
        {ok, Value} ->
          ValidationOptions = #{null_member_handling => remove,
                                disable_verification => true},
          case jsv:validate(Value, JSVDefinition, ValidationOptions) of
            {ok, Resource} ->
              {ok, Resource};
            {error, Errors} ->
              {error, {invalid_response_body, Status,
                       {invalid_resource_data, Errors}}}
          end;
        {error, Reason} ->
          {error, {invalid_response_body, Status,
                   {invalid_json_data, Reason}}}
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
definition(core_v1_secret) ->
  #{path_name => <<"secrets">>};
definition(networking_v1_network_policy) ->
  #{path_name => <<"networkpolicies">>,
    group => <<"networking.k8s.io">>,
    version => <<"v1">>};
definition(io_k8s_metrics_pkg_apis_metrics_v1beta1_pod_metrics) ->
  #{path_name => <<"pods">>,
    group => <<"metrics.k8s.io">>,
    version => <<"v1beta1">>};
definition(Id) ->
  error({unknown_resource, Id}).

-spec delete_response_id(id()) -> id().
delete_response_id(Id) ->
  %% The API is inconsistent: deleting a resource usually yields a Status
  %% object, but not always.
  %%
  %% Additionally, even though the OpenAPI specification indicates that
  %% deleting a namespace should yield a Status object, it does not and yield
  %% a Namespace object.
  Ids = [core_v1_namespace,
         core_v1_persistent_volume_claim,
         core_v1_pod,
         core_v1_pod_template,
         core_v1_resource_quota,
         core_v1_service_account,
         core_v1_persistent_volume,
         policy_v1beta1_pod_security_policy,
         storage_v1_csi_driver,
         storage_v1_csi_node,
         storage_v1_storage_class,
         storage_v1_volume_attachment,
         storage_v1alpha1_volume_attachment,
         storage_v1beta1_csi_driver,
         storage_v1beta1_csi_node,
         storage_v1beta1_storage_class,
         storage_v1beta1_volume_attachment],
  case lists:member(Id, Ids) of
    true ->
      Id;
    false ->
      apimachinery_apis_meta_v1_status
  end.

-spec add_options_query(options(), binary(), binary()) -> options().
add_options_query(Options, Name, Value) ->
  Parameter = {Name, Value},
  case maps:find(query, Options) of
    {ok, Query} ->
      Options#{query => [Parameter | Query]};
    error ->
      Options#{query => [Parameter]}
  end.
