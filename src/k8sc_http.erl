-module(k8sc_http).

-export([pool_options/1, pool_options/2]).

-spec pool_options(k8sc_config:config()) ->
        {ok, mhttp_pool:options()} | {error, term()}.
pool_options(Config = #{current_context := ContextName}) ->
  pool_options(Config, ContextName);
pool_options(_) ->
  {error, missing_context}.

-spec pool_options(k8sc_config:config(), k8sc_config:context_name()) ->
        {ok, mhttp_pool:options()} | {error, term()}.
pool_options(Config = #{contexts := Contexts,
                        clusters := Clusters,
                        users := Users},
             ContextName) ->
  try
    case maps:find(ContextName, Contexts) of
      {ok, #{cluster := ClusterName, user := UserName}} ->
        Cluster = case maps:find(ClusterName, Clusters) of
                    {ok, C} -> C;
                    error -> throw({error, {unknown_cluster, ClusterName}})
                  end,
        User = case maps:find(UserName, Users) of
                 {ok, U} -> U;
                 error -> throw({error, {unknown_user, UserName}})
               end,
        {ok, pool_options(Config, Cluster, User)};
      error ->
        throw({error, {unknown_context, ContextName}})
    end
  catch
    throw:{error, Reason} ->
      error(Reason)
  end.

-spec pool_options(k8sc_config:config(), k8sc_config:cluster(),
                   k8sc_config:user()) ->
        mhttp_pool:options().
pool_options(Config, Cluster, User) ->
  #{client_options => client_options(Config, Cluster, User)}.

-spec client_options(k8sc_config:config(), k8sc_config:cluster(),
                     k8sc_config:user()) ->
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
                  [{key, read_private_keys(Value)} | Acc];
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

-spec read_private_keys(binary()) -> [ssl:key()].
read_private_keys(Base64Data) ->
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
    Keys ->
      Keys
  end.

-spec decode_base64(binary()) -> binary().
decode_base64(Data) ->
  try
    base64:decode(Data)
  catch
    error:_ ->
      throw({error, invalid_base64_data})
  end.
