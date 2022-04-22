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

-module(k8s_network_policies).

-export([get/2, create/2, delete/2,
         options/1]).

-export_type([network_policy/0]).

-type network_policy() :: k8s_model:networking_v1_network_policy().

-spec get(binary(), k8s_resources:options()) -> k8s:result(network_policy()).
get(Name, Options) ->
  k8s_resources:get(networking_v1_network_policy, Name, options(Options)).

-spec create(network_policy(), k8s_resources:options()) ->
        k8s:result(network_policy()).
create(Policy, Options) ->
  k8s_resources:create(networking_v1_network_policy, Policy, options(Options)).

-spec delete(binary(), k8s_resources:options()) ->
        k8s:result(network_policy()).
delete(Name, Options) ->
  k8s_resources:delete(networking_v1_network_policy, Name, options(Options)).

-spec options(k8s_resources:options()) -> k8s_resources:options().
options(Options) ->
  set_default_namespace(Options).

-spec set_default_namespace(k8s_resources:options()) ->
        k8s_resources:options().
set_default_namespace(Options) when not is_map_key(namespace, Options) ->
  Options#{namespace => <<"default">>};
set_default_namespace(Options) ->
  Options.
