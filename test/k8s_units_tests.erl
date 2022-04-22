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

-module(k8s_units_tests).

-include_lib("eunit/include/eunit.hrl").

parse_cpu_test_() ->
  Parse =
    fun (String) ->
        k8s_units:parse_cpu(unicode:characters_to_binary(String))
    end,
  [?_assertEqual({ok, 0.0},
                 Parse("0")),
   ?_assertEqual({ok, 1.0},
                 Parse("1")),
   ?_assertEqual({ok, 4.0},
                 Parse("4")),
   ?_assertEqual({ok, 0.1},
                 Parse("0.1")),
   ?_assertEqual({ok, 0.1},
                 Parse("100m")),
   ?_assertEqual({ok, 0.25},
                 Parse("250000000n")),
   ?_assertEqual({error, empty_string},
                 Parse("")),
   ?_assertEqual({error, invalid_format},
                 Parse("foo")),
   ?_assertEqual({error, {invalid_unit, <<"t">>}},
                 Parse("2t"))].

parse_memory_test_() ->
  Parse =
    fun (String) ->
        k8s_units:parse_memory(unicode:characters_to_binary(String))
    end,
  [?_assertEqual({ok, 0},
                 Parse("0")),
   ?_assertEqual({ok, 100},
                 Parse("100")),
   ?_assertEqual({ok, 5_000},
                 Parse("5k")),
   ?_assertEqual({ok, 1_500_000},
                 Parse("1.5M")),
   ?_assertEqual({ok, 1_572_864},
                 Parse("1.5Mi")),
   ?_assertEqual({ok, 2_000_000},
                 Parse("0.002G")),
   ?_assertEqual({ok, 2_147_484},
                 Parse("0.002Gi")),
   ?_assertEqual({error, empty_string},
                 Parse("")),
   ?_assertEqual({error, invalid_format},
                 Parse("foo")),
   ?_assertEqual({error, {invalid_unit, <<"t">>}},
                 Parse("2t"))].
