-module(k8s_resources_tests).

-include_lib("eunit/include/eunit.hrl").

resources_test_() ->
  {setup,
   fun () ->
       {ok, _} = application:ensure_all_started(k8s)
   end,
   fun (_) ->
       error_logger:tty(false),
       ok = application:stop(k8s),
       error_logger:tty(true)
   end,
   [fun get_resource/0,
    fun get_unknown_resource/0,
    fun list_resources/0,
    fun create_delete_resources/0]}.

get_resource() ->
  ?assertMatch({ok, #{kind := <<"Namespace">>,
                      metadata := #{name := <<"default">>}}},
               k8s_resources:get(<<"default">>, namespace_v1)).

get_unknown_resource() ->
  ?assertMatch({error, {request_failure, #{code := 404}}},
               k8s_resources:get(<<"does_not_exist">>, namespace_v1)).

list_resources() ->
  ?assertMatch({ok, #{kind := <<"NamespaceList">>,
                      items := _}},

               k8s_resources:list(namespace_list_v1)).

create_delete_resources() ->
  Name = <<"test-", (integer_to_binary(os:system_time()))/binary>>,
  ?assertMatch({ok, #{kind := <<"Namespace">>,
                      metadata := #{name := Name}}},
               k8s_resources:create(#{kind => <<"Namespace">>,
                                      apiVersion => <<"v1">>,
                                      metadata => #{name => Name}},
                                    namespace_v1)),
  ?assertMatch({ok, #{kind := <<"Namespace">>,
                      metadata := #{name := Name},
                      status := #{phase := <<"Terminating">>}}},
               k8s_resources:delete(Name, namespace_v1)).
