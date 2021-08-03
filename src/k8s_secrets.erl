-module(k8s_secrets).

-export([get/2, create/2, delete/2,
         options/1]).

-export_type([secret/0]).

-type secret() :: k8s_model:core_v1_secret().

-spec get(binary(), k8s_resources:options()) -> k8s:result(secret()).
get(Name, Options) ->
  k8s_resources:get(core_v1_secret, Name, options(Options)).

-spec create(secret(), k8s_resources:options()) ->
        k8s:result(secret()).
create(Secret, Options) ->
  k8s_resources:create(core_v1_secret, Secret, options(Options)).

-spec delete(binary(), k8s_resources:options()) -> k8s:result(secret()).
delete(Name, Options) ->
  k8s_resources:delete(core_v1_secret, Name, options(Options)).

-spec options(k8s_resources:options()) -> k8s_resources:options().
options(Options) ->
  set_default_namespace(Options).

-spec set_default_namespace(k8s_resources:options()) ->
        k8s_resources:options().
set_default_namespace(Options) when not is_map_key(namespace, Options) ->
  Options#{namespace => <<"default">>};
set_default_namespace(Options) ->
  Options.
