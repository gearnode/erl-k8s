-module(k8s_http).

-export([pool_ids/1, pools/1, pool_options/1, pool_options/2,
         send_request/1, send_request/2]).

-export_type([request_options/0]).

-type request_options() ::
        #{context => k8s_config:context_name()}.

-spec pool_ids(k8s_config:config()) -> [mhttp:pool_id()].
pool_ids(#{contexts := Contexts}) ->
  maps:fold(fun (Name, _, Acc) ->
                [pool_id(Name) | Acc]
            end, [], Contexts).

-spec pools(k8s_config:config()) ->
        {ok, [{mhttp:pool_id(), mhttp_pool:options()}]} | {error, term()}.
pools(Config = #{contexts := Contexts}) ->
  try
    Pools = maps:fold(fun (Name, _, Acc) ->
                          case pool_options(Config, Name) of
                            {ok, Options} ->
                              [{pool_id(Name), Options} | Acc];
                            {error, Reason} ->
                              throw({error, {invalid_context, Reason, Name}})
                          end
                      end, [], Contexts),
    {ok, Pools}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec pool_id(k8s_config:context_name()) -> mhttp:pool_id().
pool_id(Name) ->
  binary_to_atom(<<"k8s_", Name/binary>>).

-spec pool_options(k8s_config:config()) ->
        {ok, mhttp_pool:options()} | {error, term()}.
pool_options(Config = #{current_context := ContextName}) ->
  pool_options(Config, ContextName);
pool_options(_) ->
  {error, missing_context}.

-spec pool_options(k8s_config:config(), k8s_config:context_name()) ->
        {ok, mhttp_pool:options()} | {error, term()}.
pool_options(Config, ContextName) ->
  case k8s_config:context(ContextName, Config) of
    {ok, #{cluster := ClusterName, user := UserName}} ->
      try
        Cluster = case k8s_config:cluster(ClusterName, Config) of
                    {ok, C} -> C;
                    error -> throw({error, {unknown_cluster, ClusterName}})
                  end,
        User = case k8s_config:user(UserName, Config) of
                 {ok, U} -> U;
                 error -> throw({error, {unknown_user, UserName}})
               end,
        {ok, pool_options(Config, Cluster, User)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    error ->
      {error, unknown_context}
  end.

-spec pool_options(k8s_config:config(), k8s_config:cluster(),
                   k8s_config:user()) ->
        mhttp_pool:options().
pool_options(Config, Cluster, User) ->
  #{client_options => client_options(Config, Cluster, User)}.

-spec client_options(k8s_config:config(), k8s_config:cluster(),
                     k8s_config:user()) ->
        mhttp_client:options().
client_options(_Config, Cluster, User) ->
  ConnectOptions0 = [{verify, verify_peer}],
  ConnectOptions1 =
    maps:fold(fun
                (tls_server_name, Value, Acc) ->
                  [{server_name_indication, binary_to_list(Value)} | Acc];
                (insecure_skip_tls_verify, _, Acc) ->
                  [{verify, verify_none} | lists:keydelete(verify, 1, Acc)];
                (certificate_authority_data, Value, Acc) ->
                  [{cacerts, read_certificates(Value)} | Acc];
                (proxy_url, _, _) ->
                  throw({unsupported_cluster_setting, proxy_url});
                (_, _, Acc) ->
                 Acc
             end, ConnectOptions0, Cluster),
  ConnectOptions2 =
    maps:fold(fun
                (client_certificate_data, Value, Acc) ->
                  [{cert, read_certificates(Value)} | Acc];
                (client_key_data, Value, Acc) ->
                  [{key, read_private_key(Value)} | Acc];
                (_, _, Acc) ->
                  Acc
             end, ConnectOptions1, User),
  #{connect_options => ConnectOptions2,
    compression => true,
    log_requests => true}.

-spec read_certificates(binary()) -> [public_key:der_encoded()].
read_certificates(Base64Data) ->
  PEMData = decode_base64(Base64Data),
  case
    lists:filtermap(fun
                      ({Type, Data, _}) when Type =:= 'Certificate';
                                             Type =:= 'CertificateList' ->
                        {true, Data};
                      (_) ->
                        false
                    end, public_key:pem_decode(PEMData))
  of
    [] ->
      throw({error, {no_certificate_found}});
    Certs ->
      Certs
  end.

-spec read_private_key(binary()) -> ssl:key().
read_private_key(Base64Data) ->
  PEMData = decode_base64(Base64Data),
  case
    lists:filtermap(fun
                      ({Key, Data, _}) when Key =:= 'RSAPrivateKey';
                                            Key =:= 'DSAPrivateKey';
                                            Key =:= 'ECPrivateKey';
                                            Key =:= 'PrivateKeyInfo' ->
                        {true, {Key, Data}};
                      (_) ->
                        false
                    end, public_key:pem_decode(PEMData))
  of
    [] ->
      throw({error, {no_private_key_found}});
    [Key] ->
      Key;
    _ ->
      throw({error, {multiple_private_keys_found}})
  end.

-spec decode_base64(binary()) -> binary().
decode_base64(Data) ->
  try
    base64:decode(Data)
  catch
    error:_ ->
      throw({error, invalid_base64_data})
  end.

-spec send_request(mhttp:request()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Request) ->
  send_request(Request, #{}).

-spec send_request(mhttp:request(), request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Request, Options) ->
  Config = persistent_term:get(k8s_config),
  case maps:find(context, Options) of
    {ok, Name} ->
      case k8s_config:context(Name, Config) of
        {ok, Context} ->
          send_request(Request, Context, Config, Options);
        error ->
          {error, {unknown_context, Name}}
      end;
    error ->
      Context = k8s_config:default_context(Config),
      send_request(Request, Context, Config, Options)
  end.

-spec send_request(mhttp:request(), k8s_config:context(),
                   k8s_config:config(), request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Request, #{name := ContextName,
                        cluster := ClusterName},
             Config, _Options) ->
  case k8s_config:cluster(ClusterName, Config) of
    {ok, Cluster} ->
      PoolId = pool_id(ContextName),
      TargetRef = mhttp_request:target_uri(Request),
      TargetBase = k8s_config:cluster_uri(Cluster),
      Target = uri:resolve_reference(TargetRef, TargetBase),
      Header = [{<<"Content-Type">>, <<"application/json">>},
                {<<"Accept">>, <<"application/json">>}],
      mhttp:send_request(Request#{target => Target,
                                  header => Header},
                         #{pool => PoolId});
    error ->
      {error, {unknown_cluster, ClusterName}}
  end.