-module(k8s_exec_tests).

-include_lib("eunit/include/eunit.hrl").

exec_test_() ->
  {setup,
   fun () ->
       {ok, _} = application:ensure_all_started(k8s),
       PodName = pod_name(),
       create_pod(PodName),
       wait_until_pod_is_started(PodName),
       PodName
   end,
   fun (PodName) ->
       delete_pod(PodName),
       error_logger:tty(false),
       ok = application:stop(k8s),
       ok = application:stop(mhttp),
       error_logger:tty(true)
   end,
   fun (PodName) ->
       {timeout, 600,
        {with, PodName,
         [fun stdout_only/1,
          fun mixed_stdout_stderr/1]}}
   end}.

%% TODO tests: unknown pod, unknown program

stdout_only(PodName) ->
  Command = [<<"uname">>, <<"-s">>],
  {ok, _} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertEqual([{stdout, <<"Linux\n">>}],
               k8s_exec:receive_messages()).

mixed_stdout_stderr(PodName) ->
  Command = [<<"sh">>, <<"-c">>, <<"echo foo; echo bar >&2; echo baz">>],
  {ok, _} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertEqual([{stdout, <<"foo\nbaz\n">>},
                {stderr, <<"bar\n">>}],
               k8s_exec:receive_messages()).

create_pod(Name) ->
  Pod = #{kind => <<"Pod">>,
          apiVersion => <<"v1">>,
          metadata =>
            #{name => Name},
          spec =>
            #{restartPolicy => <<"Never">>,
              containers =>
                [#{name => <<"main">>,
                   image => <<"alpine:latest">>,
                   command => [<<"sh">>],
                   args => [<<"-c">>, <<"sleep 120">>]}]}},
  k8s_pods:create(Pod, #{}).

delete_pod(Name) ->
  k8s_pods:delete(Name, #{}).

wait_until_pod_is_started(Name) ->
  {ok, Pod} = k8s_pods:get(Name, #{}),
  case k8s_pods:all_containers_started(Pod) of
    true ->
      ok;
    false ->
      timer:sleep(1000),
      wait_until_pod_is_started(Name)
  end.

pod_name() ->
  Now = erlang:system_time(microsecond),
  <<"test-", (integer_to_binary(Now))/binary>>.
