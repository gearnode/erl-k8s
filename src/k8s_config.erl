-module(k8s_config).

-export([cluster/2, cluster_uri/1, user/2, context/2, default_context/1,
         default_path/0, load/0, load/1, jsv_catalog/0]).

-export_type([error_reason/0,
              config/0,
              cluster_name/0, cluster/0,
              user_name/0, user/0,
              context_name/0, context/0]).

-type error_reason() ::
        kubectl_not_found
      | {kubectl_signal, Signo :: pos_integer(), Output :: binary()}
      | {kubectl_exit, Status :: pos_integer(), Output :: binary()}
      | {kubectl_io, PosixCode :: term, Output :: binary()} % XXX file:posix() ?
      | {invalid_json_data, json:error()}
      | {invalid_data, [jsv:value_error()]}.

%% See
%% https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/
%% for more information.

-type config() ::
        #{clusters := #{cluster_name() := cluster()},
          users := #{user_name() := user()},
          contexts := #{context_name() := context()},
          current_context => context_name()}.

-type cluster_name() :: binary().
-type cluster() ::
        #{name := cluster_name(),
          server := binary(),
          tls_server_name => binary(),
          insecure_skip_tls_verify => boolean(),
          certificate_authority_data => binary(),
          proxy_url => binary()}.

-type user_name() :: binary().
-type user() ::
        #{name := user_name(),
          client_certificate_data => binary(),
          client_key_data => binary()}.

-type context_name() :: binary().
-type context() ::
        #{name := context_name(),
          cluster := cluster_name(),
          user := user_name(),
          namespace => binary()}.

-spec cluster(cluster_name(), config()) -> {ok, cluster()} | error.
cluster(Name, #{clusters := Clusters}) ->
  maps:find(Name, Clusters).

-spec cluster_uri(cluster()) -> uri:uri().
cluster_uri(#{server := URIString}) ->
  {ok, URI} = uri:parse(URIString),
  URI.

-spec user(user_name(), config()) -> {ok, user()} | error.
user(Name, #{users := Users}) ->
  maps:find(Name, Users).

-spec context(context_name(), config()) -> {ok, context()} | error.
context(Name, #{contexts := Contexts}) ->
  maps:find(Name, Contexts).

-spec default_context(config()) -> context().
default_context(Config = #{current_context := Name}) ->
  {ok, Context} = context(Name, Config),
  Context;
default_context(#{contexts := Contexts}) when map_size(Contexts) > 0 ->
  {_, Context, _} = maps:next(maps:iterator(Contexts)),
  Context.

-spec default_path() -> file:name().
default_path() ->
  case os:getenv("KUBECONFIG") of
    false ->
      {ok, Dir} = init:get_argument(home),
      filename:join([Dir, ".kube", "config"]);
    Path ->
      Path
  end.

-spec load() -> {ok, config()} | {error, error_reason()}.
load() ->
  load(default_path()).

-spec load(file:name_all()) -> {ok, config()} | {error, error_reason()}.
load(ConfigPath) ->
  case os:find_executable("kubectl") of
    false ->
      {error, kubectl_not_found};
    KubectlPath ->
      Args = ["kubectl", "config", "view", "--kubeconfig", ConfigPath,
              "--raw", "-o", "json"],
      try
        Data = exec_program(KubectlPath, Args),
        {ok, read(Data)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end
  end.

-spec read(binary()) -> config().
read(Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      read_value(Value);
    {error, Reason} ->
      throw({error, {invalid_json_data, Reason}})
  end.

-spec read_value(json:value()) -> config().
read_value(Value) ->
  Options = #{disable_verification => true,
              invalid_member_handling => keep,
              null_member_handling => remove},
  case jsv:validate(Value, {ref, k8s_config, config}, Options) of
    {ok, ConfigData0} ->
      ConfigData = normalize_keys(ConfigData0),
      Items = fun (Key, ItemName) ->
                  extract_named_items(maps:get(Key, ConfigData, []), ItemName)
              end,
      Config = #{clusters => Items(clusters, cluster),
                 users => Items(users, user),
                 contexts => Items(contexts, context)},
      maps:merge(Config, maps:with([current_context], ConfigData));
    {error, Reason} ->
      throw({error, {invalid_data, Reason}})
  end.

-spec extract_named_items(json:array(), ItemName :: atom()) -> map().
extract_named_items(ItemData, ItemName) ->
  lists:foldl(fun (#{name := Name, ItemName := ItemValue}, Acc) ->
                  Acc#{Name => ItemValue#{name => Name}}
              end, #{}, ItemData).

-spec normalize_keys(term()) -> term().
normalize_keys(Term) when is_map(Term) ->
  maps:fold(fun (Key, Value, Acc) ->
                Acc#{normalize_key(Key) => normalize_keys(Value)}
            end, #{}, Term);
normalize_keys(Term) when is_list(Term) ->
  lists:map(fun normalize_keys/1, Term);
normalize_keys(Term) ->
  Term.

-spec normalize_key(binary() | atom()) -> binary() | atom().
normalize_key(Key) when is_atom(Key) ->
  Bin = atom_to_binary(Key),
  Bin2 = string:replace(Bin, <<"-">>, <<"_">>, all),
  binary_to_atom(list_to_binary(Bin2));
normalize_key(Key) when is_binary(Key) ->
  Key.

-spec exec_program(file:name_all(), [string() | binary()]) -> binary().
exec_program(Path, [Arg0 | Args]) ->
  %% Note that open_port/2 throws an exception when it fails
  Options = [{arg0, Arg0}, {args, Args},
             stderr_to_stdout, exit_status, binary],
  try
    Port = open_port({spawn_executable, Path}, Options),
    try
      read_program_output(Port, <<>>)
    after
      catch port_close(Port)
    end
  catch
    throw:{error, Reason} ->
      throw({error, Reason});
    throw:Error ->
      throw({error, Error})
  end.

-spec read_program_output(port(), binary()) -> binary().
read_program_output(Port, Acc) ->
  receive
    {Port, {exit_status, 0}} ->
      Acc;
    {Port, {exit_status, Status}} when Status > 128 ->
      throw({error, {kubectl_signal, Status - 128, Acc}});
    {Port, {exit_status, Status}} ->
      throw({error, {kubectl_exit, Status, Acc}});
    {Port, {data, Data}} ->
      read_program_output(Port, <<Acc/binary, Data/binary>>);
    {'EXIT', Port, PosixCode} ->
      throw({error, {kubectl_io, PosixCode, Acc}})
  end.

-spec jsv_catalog() -> jsv:catalog().
jsv_catalog() ->
  #{config =>
      {object,
       #{members =>
           #{clusters => {array, #{element => {ref, cluster_entry}}},
             contexts => {array, #{element => {ref, context_entry}}},
             users => {array, #{element => {ref, user_entry}}},
             'current-context' => string}}},
    %% Clusters
    cluster_entry =>
      {object,
       #{members =>
           #{name => string,
             cluster => {ref, cluster}},
         required =>
           [name, cluster]}},
    cluster =>
      {object,
       #{members =>
           #{server => string,
             'tls-server-name' => string,
             'certificate-authority-data' => string,
             'insecure-skip-tls-verify' => boolean,
             'proxy-url' => string},
        required =>
           [server]}},
    %% Contexts
    context_entry =>
      {object,
       #{members =>
           #{name => string,
             context => {ref, context}},
         required =>
           [name, context]}},
    context =>
      {object,
       #{members =>
           #{cluster => string,
             user => string,
             namespace => string},
         required =>
           [cluster, user]}},
    %% Users
    user_entry =>
      {object,
       #{members =>
           #{name => string,
             user => {ref, user}},
         required =>
           [name, user]}},
    user =>
      {object,
       #{members =>
           #{'client-certificate-data' => string,
             'client-key-data' => string},
         required =>
           []}}}.
