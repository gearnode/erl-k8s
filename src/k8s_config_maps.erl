%% Copyright (c) 2021 Exograd SAS.
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

-module(k8s_config_maps).

-export([get/2, create/2, delete/2,
         options/1]).

-export_type([config_map/0]).

-type config_map() :: k8s_model:core_v1_config_map().

-spec get(binary(), k8s_resources:options()) -> k8s:result(config_map()).
get(Name, Options) ->
  k8s_resources:get(core_v1_config_map, Name, options(Options)).

-spec create(config_map(), k8s_resources:options()) ->
        k8s:result(config_map()).
create(ConfigMap, Options) ->
  k8s_resources:create(core_v1_config_map, ConfigMap, options(Options)).

-spec delete(binary(), k8s_resources:options()) -> k8s:result(config_map()).
delete(Name, Options) ->
  k8s_resources:delete(core_v1_config_map, Name, options(Options)).

-spec options(k8s_resources:options()) -> k8s_resources:options().
options(Options) ->
  set_default_namespace(Options).

-spec set_default_namespace(k8s_resources:options()) ->
        k8s_resources:options().
set_default_namespace(Options) when not is_map_key(namespace, Options) ->
  Options#{namespace => <<"default">>};
set_default_namespace(Options) ->
  Options.
