-module(k8sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
  try
    register_jsv_catalogs(),
    Config = load_config(),
    start_mhttp_pool(Config),
    k8sc_sup:start_link()
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  stop_mhttp_pool(),
  unregister_jsv_catalogs(),
  ok.

-spec load_config() -> k8sc_config:config().
load_config() ->
  case k8sc_config:load() of
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
  [{k8sc, k8sc_jsv:catalog()},
   {k8sc_config, k8sc_config:jsv_catalog()}].

-spec start_mhttp_pool(k8sc_config:config()) -> ok.
start_mhttp_pool(Config) ->
  case k8sc_http:pool_options(Config) of
    {ok, PoolOptions} ->
      case mhttp_pool_sup:start_pool(k8sc, PoolOptions) of
        {ok, _} ->
          ok;
        {error, Reason} ->
          throw({error, {start_mhttp_pool, Reason}})
      end;
    {error, Reason} ->
      throw({error, {invalid_configuration, Reason}})
  end.

-spec stop_mhttp_pool() -> ok.
stop_mhttp_pool() ->
  mhttp_pool:stop(k8sc).
