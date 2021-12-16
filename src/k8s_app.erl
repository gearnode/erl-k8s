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

-module(k8s_app).

-behaviour(application).

-export([start/2, stop/1]).

-export_type([options/0]).

-type options() ::
        #{field_manager => binary()}.

start(_StartType, _Args) ->
  try
    register_jsv_catalogs(),
    Options = application:get_env(k8s, options, #{}),
    persistent_term:put(k8s_options, Options),
    Config = load_config(),
    persistent_term:put(k8s_config, Config),
    start_mhttp_pools(Config),
    k8s_sup:start_link()
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  Config = persistent_term:get(k8s_config),
  stop_mhttp_pools(Config),
  unregister_jsv_catalogs(),
  ok.

-spec load_config() -> k8s_config:config().
load_config() ->
  case k8s_config:load() of
    {ok, Config} ->
      Config;
    {error, Reason} ->
      throw({error, {load_config, Reason}})
  end.

-spec register_jsv_catalogs() -> ok | {error, term()}.
register_jsv_catalogs() ->
  Catalogs = catalogs(),
  lists:foreach(fun ({Name, Catalog}) ->
                    jsv:register_catalog(Name, Catalog)
                end, Catalogs),
  lists:foreach(fun ({Name, _}) ->
                    case jsv:verify_catalog(Name, #{}) of
                      ok ->
                        ok;
                      {error, Reason} ->
                        throw({error, {invalid_jsv_catalog, Reason, Name}})
                    end
                end, Catalogs).

-spec unregister_jsv_catalogs() -> ok.
unregister_jsv_catalogs() ->
  Catalogs = catalogs(),
  lists:foreach(fun ({Name, _}) ->
                    jsv:unregister_catalog(Name)
                end, Catalogs),
  ok.

-spec catalogs() -> [{jsv:catalog_name(), jsv:catalog()}].
catalogs() ->
  [{k8s_config, k8s_config:jsv_catalog()},
   {k8s, k8s_jsv:catalog()}].

-spec start_mhttp_pools(k8s_config:config()) -> ok.
start_mhttp_pools(Config) ->
  case k8s_http:pools(Config) of
    {ok, Pools} ->
      lists:foreach(fun ({Id, Options}) ->
                        start_mhttp_pool(Id, Options)
                    end, Pools);
    {error, Reason} ->
      throw({error, {invalid_configuration, Reason}})
  end.

-spec start_mhttp_pool(mhttp:pool_id(), mhttp_pool:options()) -> ok.
start_mhttp_pool(Id, Options) ->
  case mhttp_pool_sup:start_pool(Id, Options) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      throw({error, {start_mhttp_pool, Reason, Id}})
  end.

-spec stop_mhttp_pools(k8s_config:config()) -> ok.
stop_mhttp_pools(Config) ->
  Ids = k8s_http:pool_ids(Config),
  lists:foreach(fun mhttp_pool_sup:stop_pool/1, Ids).
