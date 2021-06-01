-module(k8s_exec).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2, start_link/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([pod_name/0, command/0,
              message/0,
              ref/0, options/0]).

-type pod_name() :: binary().
-type command() :: [binary()].

-type message() ::
        {stdout, binary()}
      | {stderr, binary()}
      | {control, binary()}.

-type ref() :: et_gen_server:ref().

-type options() ::
        #{context => k8s_config:context_name(),
          namespace => binary(),
          container => binary()}.

-type state() :: #{options := options(),
                   pod := pod_name(),
                   command := command(),
                   websocket_client := pid()}.

-spec start_link(pod_name(), command()) -> {ok, pid()} | {error, term()}.
start_link(Pod, Command) ->
  start_link(Pod, Command, #{}).

-spec start_link(pod_name(), command(), options()) ->
        {ok, pid()} | {error, term()}.
start_link(Pod, Command, Options) ->
  gen_server:start_link(?MODULE, [Pod, Command, Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Pod, Command, Options]) ->
  Namespace = maps:get(namespace, Options, <<"default">>),
  logger:update_process_metadata(#{domain => [k8s, exec],
                                   namespace => Namespace,
                                   pod => Pod}),
  case connect(Pod, Command, Options) of
    {ok, Pid} ->
      %% TODO link
      State = #{options => Options,
                pod => Pod,
                command => Command,
                websocket_pid => Pid},
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
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
handle_info({websocket, connected}, State) ->
  ?LOG_DEBUG("connection established"),
  {noreply, State};
handle_info({websocket, terminating}, State) ->
  ?LOG_DEBUG("connection closed"),
  {stop, normal, State};
handle_info({websocket, {message, {data, _, Data}}}, State) ->
  case parse_message(Data) of
    {ok, {_, <<"">>}} ->
      %% The point of these empty messages is unknown; we skip them for the
      %% time being.
      {noreply, State};
    {ok, Message} ->
      ?LOG_DEBUG("message: ~0tp", [Message]),
      %% TODO
      {noreply, State};
    {error, Reason} ->
      ?LOG_ERROR("invalid message: ~tp", [Data]),
      {stop, {error, Reason}, State}
  end;
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec connect(pod_name(), command(), options()) ->
        {ok, pid()} | {error, term()}.
connect(Pod, Command, Options) ->
  Header = [{<<"Accept">>, <<"application/json, */*">>},
            {<<"X-Stream-Protocol-Version">>, <<"v4.channel.k8s.io">>}],
  Request = #{method => <<"GET">>,
              target => uri(Pod, Command, Options),
              header => Header},
  RequestOptions = #{},
  case k8s_http:request_and_pool(Request, RequestOptions) of
    {ok, {Request2, PoolId}} ->
      mhttp_websocket:connect(Request2, RequestOptions#{pool => PoolId});
    {error, Reason} ->
      {error, Reason}
  end.

-spec uri(pod_name(), command(), options()) -> uri:uri().
uri(Pod, Command, Options) ->
  ResourceOptions = maps:with([context, namespace], Options),
  BasePath = k8s_resources:path(core_v1_pod, Pod, ResourceOptions),
  Path = <<BasePath/binary, "/exec">>,
  Query1 = [{<<"stdout">>, <<"true">>},
            {<<"stderr">>, <<"true">>}],
  Query2 = case maps:find(container, Options) of
             {ok, Container} -> [{<<"container">>, Container}];
             error -> []
           end,
  Query = Query1 ++ Query2 ++ [{<<"command">>, Part} || Part <- Command],
  #{path => Path, query => Query}.

-spec parse_message(binary()) -> k8s:result(message()).
parse_message(<<1:8, Data/binary>>) ->
  {ok, {stdout, Data}};
parse_message(<<2:8, Data/binary>>) ->
  {ok, {stderr, Data}};
parse_message(<<3:8, Data/binary>>) ->
  {ok, {control, Data}};
parse_message(Data) ->
  {error, {invalid_exec_message, Data}}.

