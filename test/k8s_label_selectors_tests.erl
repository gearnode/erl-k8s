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

-module(k8s_label_selectors_tests).

-include_lib("eunit/include/eunit.hrl").

format_test_() ->
  Format = fun k8s_label_selectors:format/1,
  [?_assertEqual(<<"">>,
                 Format(#{})),
   ?_assertEqual(<<"foo=bar">>,
                 Format(#{matchLabels =>
                            #{<<"foo">> => <<"bar">>}})),
   ?_assertEqual(<<"a=1,b=2,c=3">>,
                 Format(#{matchLabels =>
                            #{<<"a">> => <<"1">>,
                              <<"b">> => <<"2">>,
                              <<"c">> => <<"3">>}})),
   ?_assertEqual(<<"a=1,b=2,c=3">>,
                 Format(#{matchLabels =>
                            #{<<"a">> => <<"1">>,
                              <<"b">> => <<"2">>,
                              <<"c">> => <<"3">>}})),
   ?_assertEqual(<<"foo">>,
                 Format(#{matchExpressions =>
                            [#{key => <<"foo">>,
                               operator => <<"Exists">>}]})),
   ?_assertEqual(<<"a,!b,c in (),d in (1),e notin (2,3,4)">>,
                 Format(#{matchExpressions =>
                            [#{key => <<"a">>,
                               operator => <<"Exists">>},
                             #{key => <<"b">>,
                               operator => <<"DoesNotExist">>},
                             #{key => <<"c">>,
                               operator => <<"In">>},
                             #{key => <<"d">>,
                               operator => <<"In">>,
                               values => [<<"1">>]},
                             #{key => <<"e">>,
                               operator => <<"NotIn">>,
                               values => [<<"2">>, <<"3">>, <<"4">>]}]})),
   ?_assertEqual(<<"a=1,b=2,foo in (bar)">>,
                 Format(#{matchLabels =>
                            #{<<"a">> => <<"1">>,
                              <<"b">> => <<"2">>},
                          matchExpressions =>
                            [#{key => <<"foo">>,
                               operator => <<"In">>,
                               values => [<<"bar">>]}]}))].
