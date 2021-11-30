-module(k8s_pods).

-export([all_containers_started/1,
         get/2, create/2, strategic_merge_patch/3, delete/3,
         options/1]).

-export_type([pod/0]).

-type pod() :: k8s_model:core_v1_pod().

-type delete_options() ::
   #{grace_period => non_neg_integer()}. % seconds

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

-spec delete(binary(), delete_options(), k8s_resources:options()) ->
        k8s:result(pod()).
delete(Name, DeleteOptions, Options) ->
  k8s_resources:delete(core_v1_pod, Name,
                       delete_options(DeleteOptions, Options)).

-spec delete_options(delete_options(), k8s_resources:options()) ->
        k8s_resources:options().
delete_options(DeleteOptions, Options1) ->
  F = fun
        (grace_period, Seconds, Options) ->
          k8s_resources:add_options_query(Options,
                                          <<"gracePeriodSeconds">>,
                                          integer_to_binary(Seconds))
      end,
  Options2 = maps:fold(F, Options1, DeleteOptions),
  options(Options2).

-spec options(k8s_resources:options()) -> k8s_resources:options().
options(Options) ->
  set_default_namespace(Options).

-spec set_default_namespace(k8s_resources:options()) ->
        k8s_resources:options().
set_default_namespace(Options) when not is_map_key(namespace, Options) ->
  Options#{namespace => <<"default">>};
set_default_namespace(Options) ->
  Options.
