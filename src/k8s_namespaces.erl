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

-module(k8s_namespaces).

-export([get/2, create/2, strategic_merge_patch/3, delete/2]).

-export_type([namespace/0]).

-type namespace() :: k8s_model:core_v1_namespace().

-spec get(binary(), k8s_resources:options()) -> k8s:result(namespace()).
get(Name, Options) ->
  k8s_resources:get(core_v1_namespace, Name, Options).

-spec create(namespace(), k8s_resources:options()) -> k8s:result(namespace()).
create(Namespace, Options) ->
  k8s_resources:create(core_v1_namespace, Namespace, Options).

-spec strategic_merge_patch(binary(), namespace(), k8s_resources:options()) ->
        k8s:result(namespace()).
strategic_merge_patch(Name, Namespace, Options) ->
  k8s_resources:strategic_merge_patch(core_v1_namespace, Name, Namespace,
                                      Options).

-spec delete(binary(), k8s_resources:options()) -> k8s:result(namespace()).
delete(Name, Options) ->
  k8s_resources:delete(core_v1_namespace, Name, Options).
