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
    fun get_unknown_resource/0]}.

get_resource() ->
  ?assertMatch({ok, #{kind := <<"Namespace">>,
                      metadata := #{name := <<"default">>}}},
               k8s_resources:get(<<"default">>, namespace_v1)).

get_unknown_resource() ->
  ?assertMatch({error, {request_failure, #{code := 404}}},
               k8s_resources:get(<<"does_not_exist">>, namespace_v1)).
