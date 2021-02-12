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
