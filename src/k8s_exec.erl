-module(k8s_exec).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start/2, start/3, start_link/2, start_link/3, stop/1,
         receive_messages/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([pod_name/0, command/0,
              message/0, event_message/0, event/0,
              ref/0, options/0]).

-type pod_name() :: binary().
-type command() :: [binary()].

-type event_message() :: {k8s_exec, event()}.

-type event() ::
        {message, message()}
      | terminated.

-type message() ::
        {stdout, binary()}
      | {stderr, binary()}
      | {error, error()}.

-type error() ::
        program_not_found
      | {program_failure, ExitCode :: pos_integer()}
      | {program_killed, Signal :: pos_integer()}
      | binary().

-type ref() :: et_gen_server:ref().

-type options() ::
        #{event_target => pid() | atom(),
          context => k8s_config:context_name(),
          namespace => binary(),
          container => binary()}.

-type state() :: #{options := options(),
                   pod := pod_name(),
                   command := command(),
                   websocket_client := pid()}.

-spec start(pod_name(), command()) -> {ok, pid()} | {error, term()}.
start(Pod, Command) ->
  start(Pod, Command, #{}).

-spec start(pod_name(), command(), options()) ->
        {ok, pid()} | {error, term()}.
start(Pod, Command, Options) ->
  gen_server:start(?MODULE, [Pod, Command, options(Options)], []).

-spec start_link(pod_name(), command()) -> {ok, pid()} | {error, term()}.
start_link(Pod, Command) ->
  start_link(Pod, Command, #{}).

-spec start_link(pod_name(), command(), options()) ->
        {ok, pid()} | {error, term()}.
start_link(Pod, Command, Options) ->
  gen_server:start_link(?MODULE, [Pod, Command, options(Options)], []).

-spec options(options()) -> options().
options(Options0) ->
  case maps:is_key(event_target, Options0) of
    true -> Options0;
    false -> Options0#{event_target => self()}
  end.

-spec stop(ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

-spec receive_messages(non_neg_integer()) ->
        {ok | timeout, Messages} | {error, term(), Messages} when
    Messages :: [message()].
receive_messages(Timeout) ->
  Timer = erlang:send_after(Timeout, self(), timeout),
  try
    do_receive_messages([])
  after
    erlang:cancel_timer(Timer)
  end.

-spec do_receive_messages(Messages) ->
        {ok | timeout, Messages} | {error, term(), Messages} when
    Messages :: [message()].
do_receive_messages(Messages) ->
  receive
    timeout ->
      {timeout, lists:reverse(Messages)};
    {k8s_exec, {message, Message}} ->
      do_receive_messages([Message | Messages]);
    {k8s_exec, terminated} ->
      {ok, lists:reverse(Messages)}
  end.

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Pod, Command, Options]) ->
  Namespace = maps:get(namespace, Options, <<"default">>),
  logger:update_process_metadata(#{domain => [k8s, exec],
                                   namespace => Namespace,
                                   pod => Pod}),
  case connect(Pod, Command, Options) of
    {ok, Pid} ->
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
  send_event(terminated, State),
  {stop, normal, State};
handle_info({websocket, {message, {data, _, Data}}}, State) ->
  case parse_message(Data) of
    {ok, {_, <<"">>}} ->
      %% The point of these empty messages is unknown; we skip them for the
      %% time being.
      {noreply, State};
    {ok, {error, String}} ->
      Error = parse_error_message(String),
      send_event({message, {error, Error}}, State),
      {noreply, State};
    {ok, Message} ->
      send_event({message, Message}, State),
      {noreply, State};
    {error, Reason} ->
      ?LOG_ERROR("invalid message: ~tp", [Data]),
      {stop, {error, Reason}, State}
  end;
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec send_event(event(), state()) -> ok.
send_event(Event, #{options := #{event_target := Target}}) ->
  Target ! {k8s_exec, Event},
  ok;
send_event(_, _) ->
  ok.

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
      RequestOptions2 = RequestOptions#{pool => PoolId},
      case mhttp_websocket:connect(Request2, RequestOptions2) of
        {ok, Pid} ->
          link(Pid),
          {ok, Pid};
        {error, {no_upgrade, Response}} ->
          Status = mhttp_response:status(Response),
          ErrorData = response_error_data(Response),
          {error, {exec_error, Status, ErrorData}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec response_error_data(mhttp:response()) ->
        binary() | k8s_model:apimachinery_apis_meta_v1_status().
response_error_data(Response) ->
  Body = mhttp_response:body(Response),
  case json:parse(Body) of
    {ok, Value} ->
      Definition = {ref, k8s, apimachinery_apis_meta_v1_status},
      Options = #{null_member_handling => remove,
                  disable_verification => true},
      case jsv:validate(Value, Definition, Options) of
        {ok, Status} ->
          Status;
        {error, _} ->
          Body
      end;
    {error, _} ->
      Body
  end.

-spec uri(pod_name(), command(), options()) -> uri:uri().
uri(Pod, Command, Options) ->
  Namespace = maps:get(namespace, Options, <<"default">>),
  ResourceOptions0 = maps:with([context], Options),
  ResourceOptions = ResourceOptions0#{namespace => Namespace},
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
  {ok, {error, Data}};
parse_message(Data) ->
  {error, {invalid_exec_message, Data}}.

-spec parse_error_message(binary()) -> error().
parse_error_message(String) ->
  Funs = [fun parse_program_not_found_error_message/1,
          fun parse_program_exit_error_message/1],
  parse_error_message(String, Funs).

-spec parse_error_message(binary(),
                          [fun((binary()) -> {ok, error()} | error)]) ->
        error().
parse_error_message(String, []) ->
  String;
parse_error_message(String, [Fun | Funs]) ->
  case Fun(String) of
    {ok, Error} ->
      Error;
    error ->
      parse_error_message(String, Funs)
  end.

-spec parse_program_not_found_error_message(binary()) -> {ok, error()} | error.
parse_program_not_found_error_message(String) ->
  case string:find(String, "executable file not found in \$PATH") of
    nomatch ->
      error;
    _ ->
      {ok, program_not_found}
  end.

-spec parse_program_exit_error_message(binary()) -> {ok, error()} | error.
parse_program_exit_error_message(String) ->
  RE = "exit code ([0-9]+)$",
  case re:run(String, RE, [{capture, all_but_first, binary}]) of
    {match, [CodeString]} ->
      case binary_to_integer(CodeString) of
        Code when Code > 128 ->
          Signal = Code - 128,
          {program_killed, Signal};
        Code ->
          {program_failure, Code}
      ok;
    nomatch ->
      error
  end.
