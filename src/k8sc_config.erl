-module(k8sc_config).

-export([default_path/0, load/0, load/1, jsv_catalog/0]).

-export_type([config/0,
              cluster_name/0, cluster/0,
              user_name/0, user/0,
              context_name/0, context/0]).

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
        #{server := binary(),
          tls_server_name => binary(),
          insecure_skip_tls_verify => boolean(),
          certificate_authority_data => binary(),
          proxy_url => binary()}.

-type user_name() :: binary().
-type user() ::
        #{client_certificate_data => binary(),
          client_key_data => binary()}.

-type context_name() :: binary().
-type context() ::
        #{cluster := cluster_name(),
          user := user_name(),
          namespace => binary()}.

-spec default_path() -> file:name().
default_path() ->
  case os:getenv("KUBECONFIG") of
    false ->
      {ok, Dir} = init:get_argument(home),
      filename:join(Dir, ".kube");
    Path ->
      Path
  end.

-spec load() -> {ok, config()} | {error, term()}.
load() ->
  load(default_path()).

-spec load(file:name_all()) -> {ok, config()} | {error, term()}.
load(ConfigPath) ->
  case os:find_executable("kubectl") of
    false ->
      {error, missing_kubectl};
    KubectlPath ->
      Args = ["kubectl", "config", "view", "--kubeconfig", ConfigPath,
              "--raw", "-o", "json"],
      try
        Data = exec_program(KubectlPath, Args),
        read(Data)
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
              invalid_member_handling => keep},
  case jsv:validate(Value, {ref, k8sc_config, config}, Options) of
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
                  Acc#{Name => ItemValue}
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
      throw({error, {signal, Status - 128}});
    {Port, {exit_status, Status}} ->
      throw({error, {exit, Status}});
    {Port, {data, Data}} ->
      read_program_output(Port, <<Acc/binary, Data/binary>>);
    {'EXIT', Port, PosixCode} ->
      throw({error, {port, PosixCode}})
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
