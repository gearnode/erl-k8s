-module(k8s_resource_registry).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0, resource_definition/1, resource_modules/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{table := ets:tab()}.

-spec start_link() -> et_gen_server:start_ret().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?MODULE).

-spec resource_definition(k8s_resource:type()) -> k8s_resource:definition().
resource_definition(Type) ->
  case ets:lookup(?MODULE, Type) of
    [{Type, Def}] ->
      Def;
    [] ->
      error({unknown_resource, Type})
  end.

-spec init(list()) -> et_gen_server:init_ret(state()).
init([]) ->
  logger:update_process_metadata(#{domain => [k8s, resource_registry]}),
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
  lists:foreach(fun (M) ->
                    (Def = #{type := Type}) = M:definition(),
                    ets:insert(Table, {Type, Def})
                end, resource_modules()).

-spec resource_modules() -> [module()].
resource_modules() ->
  [k8s_namespace_v1,
   k8s_namespace_list_v1,
   k8s_namespace_status_v1,
   k8s_job_v1,
   k8s_list_meta_v1,
   k8s_object_meta_v1,
   k8s_status_v1,
   k8s_status_details_v1,
   k8s_status_cause_v1].
