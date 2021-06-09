-module(k8s_pods).

-export([all_containers_started/1,
         get/2, create/2, strategic_merge_patch/3, delete/2,
         options/1]).

-export_type([pod/0]).

-type pod() :: k8s_model:core_v1_pod().

-spec all_containers_started(pod()) -> boolean().
all_containers_started(#{status := #{phase := <<"Running">>,
                                     containerStatuses := Statuses}}) ->
  F = fun
        (#{state := #{running := _}}) ->
          true;
        (#{state := #{terminated := _}}) ->
          true;
        (_) ->
          false
      end,
  lists:all(F, Statuses);
all_containers_started(_) ->
  false.

-spec get(binary(), k8s_resources:options()) -> k8s:result(pod()).
get(Name, Options) ->
  k8s_resources:get(core_v1_pod, Name, options(Options)).

-spec create(pod(), k8s_resources:options()) -> k8s:result(pod()).
create(Pod, Options) ->
  k8s_resources:create(core_v1_pod, Pod, options(Options)).

-spec strategic_merge_patch(binary(), pod(), k8s_resources:options()) ->
        k8s:result(pod()).
strategic_merge_patch(Name, Pod, Options) ->
  k8s_resources:strategic_merge_patch(core_v1_pod, Name, Pod,
                                      options(Options)).

-spec delete(binary(), k8s_resources:options()) -> k8s:result(pod()).
delete(Name, Options) ->
  k8s_resources:delete(core_v1_pod, Name, options(Options)).

-spec options(k8s_resources:options()) -> k8s_resources:options().
options(Options) ->
  set_default_namespace(Options).

-spec set_default_namespace(k8s_resources:options()) ->
        k8s_resources:options().
set_default_namespace(Options) when not is_map_key(namespace, Options) ->
  Options#{namespace => <<"default">>};
set_default_namespace(Options) ->
  Options.
