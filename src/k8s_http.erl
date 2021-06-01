-module(k8s_http).

-export([pool_ids/1, pools/1, pool_options/1, pool_options/2,
         send_request/1, send_request/2,
         request_and_pool/2]).

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
  TLSOptions0 = [{verify, verify_peer}],
  TLSOptions1 = maps:fold(fun update_tls_options_from_cluster/3,
                          TLSOptions0, Cluster),
  TLSOptions2 = maps:fold(fun update_tls_options_from_user/3,
                          TLSOptions1, User),
  #{tls_options => TLSOptions2,
    compression => true,
    log_requests => true}.

-spec update_tls_options_from_cluster(atom(), any(), [Option]) ->
        [Option] when
    Option :: mhttp_client:tls_option().
update_tls_options_from_cluster(tls_server_name, Name, Options) ->
  [{server_name_indication, binary_to_list(Name)} | Options];
update_tls_options_from_cluster(insecure_skip_tls_verify, true, Options) ->
  [{verify, verify_none} | lists:keydelete(verify, 1, Options)];
update_tls_options_from_cluster(certificate_authority_data, Data, Options) ->
  [{cacerts, read_certificates(Data)} | Options];
update_tls_options_from_cluster(proxy_url, _, _Options) ->
  throw({unsupported_cluster_setting, proxy_url});
update_tls_options_from_cluster(_, _, Options) ->
  Options.

-spec update_tls_options_from_user(atom(), any(), [Option]) ->
        [Option] when
    Option :: mhttp_client:tls_option().
update_tls_options_from_user(client_certificate_data, Data, Options) ->
  [{cert, read_certificates(Data)} | Options];
update_tls_options_from_user(client_key_data, Data, Options) ->
  [{key, read_private_key(Data)} | Options];
update_tls_options_from_user(_, _, Options) ->
  Options.

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
  case request_and_pool(Request, Options) of
    {ok, {Request2, PoolId}} ->
      case mhttp:send_request(Request2, #{pool => PoolId}) of
        {ok, Response} ->
          {ok, Response};
        {error, Reason} ->
          {error, {request_error, Reason}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec request_and_pool(mhttp:request(), request_options()) ->
        k8s:result({mhttp:request(), mhttp:pool_id()}).
request_and_pool(Request, Options) ->
  Config = persistent_term:get(k8s_config),
  case maps:find(context, Options) of
    {ok, Name} ->
      case k8s_config:context(Name, Config) of
        {ok, Context} ->
          request_and_pool(Request, Context, Config, Options);
        error ->
          {error, {unknown_context, Name}}
      end;
    error ->
      Context = k8s_config:default_context(Config),
      request_and_pool(Request, Context, Config, Options)
  end.

-spec request_and_pool(mhttp:request(), k8s_config:context(),
                       k8s_config:config(), request_options()) ->
        k8s:result({mhttp:request(), mhttp:pool_id()}).
request_and_pool(Request, #{name := ContextName,
                            cluster := ClusterName},
                 Config, _Options) ->
  case k8s_config:cluster(ClusterName, Config) of
    {ok, Cluster} ->
      PoolId = pool_id(ContextName),
      TargetRef = mhttp_request:target_uri(Request),
      TargetBase = k8s_config:cluster_uri(Cluster),
      Target = uri:resolve_reference(TargetRef, TargetBase),
      Header0 = mhttp_request:header(Request),
      ExtraHeader = [{<<"Content-Type">>, <<"application/json">>},
                     {<<"Accept">>, <<"application/json">>}],
      Header = lists:keymerge(1, lists:keysort(1, Header0),
                              lists:keysort(1, ExtraHeader)),
      {ok, {Request#{target => Target, header => Header}, PoolId}};
    error ->
      {error, {unknown_cluster, ClusterName}}
  end.
