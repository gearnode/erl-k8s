%% Copyright (c) 2020-2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

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
       error_logger:tty(true)
   end,
   fun (PodName) ->
       {timeout, 600,
        {with, PodName,
         [fun stdout_only/1,
          fun mixed_stdout_stderr/1,
          fun interruption/1,
          fun unknown_pod/1,
          fun unknown_program/1,
          fun program_failure/1,
          fun program_killed/1]}}
   end}.

stdout_only(PodName) ->
  Command = [<<"uname">>, <<"-s">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertEqual({ok, [{stdout, <<"Linux\n">>}]},
               k8s_exec:receive_messages(1000)),
  ?assertNot(is_process_alive(Pid)).

mixed_stdout_stderr(PodName) ->
  Command = [<<"sh">>, <<"-c">>, <<"echo foo; echo bar >&2; echo baz">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  {ok, Messages} = k8s_exec:receive_messages(1000),
  ?assertEqual([{stderr, <<"bar\n">>},
                {stdout, <<"foo\nbaz\n">>}],
               lists:sort(Messages)),
  ?assertNot(is_process_alive(Pid)).

interruption(PodName) ->
  Command = [<<"sleep">>, <<"10">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertEqual({timeout, []}, k8s_exec:receive_messages(10)),
  ?assert(is_process_alive(Pid)),
  k8s_exec:stop(Pid),
  ?assertNot(is_process_alive(Pid)).

unknown_pod(_PodName) ->
  Command = [<<"uname">>],
  ?assertMatch({error, {exec_error, 404,
                        #{details := #{kind := <<"pods">>,
                                       name := <<"does_not_exist">>},
                          reason := <<"NotFound">>}}},
               k8s_exec:start(<<"does_not_exist">>, Command, #{})).

unknown_program(PodName) ->
  Command = [<<"does_not_exist">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertMatch({ok, [{error, program_not_found}]},
               k8s_exec:receive_messages(1000)),
  ?assertNot(is_process_alive(Pid)).

program_failure(PodName) ->
  Command = [<<"sh">>, <<"-c">>, <<"exit 42">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertMatch({ok, [{error, {program_failure, 42}}]},
               k8s_exec:receive_messages(1000)),
  ?assertNot(is_process_alive(Pid)).

program_killed(PodName) ->
  Command = [<<"sh">>, <<"-c">>, <<"kill -9 0">>],
  {ok, Pid} = k8s_exec:start_link(PodName, Command, #{}),
  ?assertMatch({ok, [{error, {program_killed, 9}}]},
               k8s_exec:receive_messages(1000)),
  ?assertNot(is_process_alive(Pid)).

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
  k8s_pods:delete(Name, #{grace_period => 1}, #{}).

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
