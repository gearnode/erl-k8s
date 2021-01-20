-module(k8sc_resource_registry).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0, resource_def/1, resource_definitions/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([group_name/0, group_version/0,
              resource_def/0, resource_def_set/0]).

-type state() :: #{table := ets:tab()}.

-type group_name() :: binary().
-type group_version() :: binary().

-type resource_def() ::
        #{group := group_name(),
          version := group_version(),
          name := k8sc_resource:name(),
          module := module()}.
-type resource_def_set() ::
        #{k8sc_resource:type() := resource_def()}.

-spec start_link() -> et_gen_server:start_ret().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?MODULE).

-spec resource_def(k8sc_resource:type()) -> resource_def().
resource_def(Type) ->
  case ets:lookup_element(?MODULE, Type, 2) of
    false ->
      error({unknown_resource, Type});
    Def ->
      Def
  end.

-spec init(list()) -> et_gen_server:init_ret(state()).
init([]) ->
  logger:update_process_metadata(#{domain => [k8sc, resource_registry]}),
  Table = ets:new(?MODULE, [set, named_table, {read_concurrency, true}]),
  register_resource_definitions(Table),
  State = #{table => Table},
  {ok, State}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, #{table := Table}) ->
  ets:delete(Table),
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec register_resource_definitions(ets:tab()) -> ok.
register_resource_definitions(Table) ->
  Defs = resource_definitions(),
  register_resource_definitions(maps:iterator(Defs), Table).

-spec register_resource_definitions(maps:iterator(k8sc_resource:type(),
                                                  resource_def()),
                                    ets:tab()) -> ok.
register_resource_definitions(It, Table) ->
  case maps:next(It) of
    {Type, Def, It2} ->
      ets:insert(Table, {Type, Def}),
      register_resource_definitions(It2, Table);
    none ->
      ok
  end.

-spec resource_definitions() -> resource_def_set().
resource_definitions() ->
  R = fun (Group, Version, Name, Module) ->
          #{group => Group,
            version => Version,
            name => Name,
            module => Module}
      end,
  #{namespace_v1 =>
      R(<<"io.k8s.api.core">>, <<"v1">>,
        <<"Namespace">>, k8sc_namespace_v1),
    object_meta_v1 =>
      R(<<"io.k8s.apimachinery.pkg.apis.meta.">>, <<"v1">>,
        <<"ObjectMeta">>, k8sc_object_meta_v1)}.
