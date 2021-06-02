-module(k8s_resources_tests).

-include_lib("eunit/include/eunit.hrl").

resources_test_() ->
  {setup,
   fun () ->
       {ok, _} = application:ensure_all_started(k8s),
       ok
   end,
   fun (_) ->
       error_logger:tty(false),
       ok = application:stop(k8s),
       error_logger:tty(true)
   end,
   [fun basic_workflow/0]}.

basic_workflow() ->
  %% Create
  Now = erlang:system_time(microsecond),
  Name = <<"test-", (integer_to_binary(Now))/binary>>,
  NewNs = #{kind => <<"Namespace">>,
            apiVersion => <<"v1">>,
            metadata => #{name => Name}},
  {ok, Ns1} = k8s_resources:create(core_v1_namespace, NewNs, #{}),
  ?assertMatch(#{kind := <<"Namespace">>,
                 metadata := #{name := Name}}, Ns1),
  %% Get
  {ok, Ns2} = k8s_resources:get(core_v1_namespace, Name, #{}),
  ?assertMatch(#{kind := <<"Namespace">>,
                 metadata := #{name := Name}}, Ns2),
  %% List
  {ok, Nss} = k8s_resources:list(core_v1_namespace_list, #{}),
  ?assertMatch(#{kind := <<"NamespaceList">>,
                 items := _}, Nss),
  %% Update
  Metadata1 = maps:get(metadata, Ns1),
  ModifiedNs1 = Ns1#{metadata => Metadata1#{labels => #{<<"a">> => <<"1">>}}},
  {ok, Ns3} = k8s_resources:update(core_v1_namespace, Name, ModifiedNs1, #{}),
  ?assertMatch(#{kind := <<"Namespace">>,
                 metadata := #{name := Name,
                               labels := #{<<"a">> := <<"1">>}}}, Ns3),
  %% Delete
  {ok, Ns4} = k8s_resources:delete(core_v1_namespace, Name, #{}),
  ?assertMatch(#{kind := <<"Namespace">>,
                 metadata := #{name := Name,
                               labels := #{<<"a">> := <<"1">>}}}, Ns4).
