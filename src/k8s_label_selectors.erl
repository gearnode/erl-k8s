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

-module(k8s_label_selectors).

-export([format/1]).

%% Reference:
%% https://kubernetes.io/docs/concepts/overview/working-with-objects/labels/

-type selector() ::
        k8s_model:apimachinery_apis_meta_v1_label_selector().
-type expression() ::
        k8s_model:apimachinery_apis_meta_v1_label_selector_requirement().

-spec format(selector()) -> binary().
format(Selector) ->
  iolist_to_binary(format_selector(Selector)).

-spec format_selector(selector()) -> iodata().
format_selector(Selector) ->
  Labels = maps:get(matchLabels, Selector, #{}),
  Expressions = maps:get(matchExpressions, Selector, []),
  Parts0 = [format_labels(Labels), format_expressions(Expressions)],
  Parts = [P || P <- Parts0, P =/= []],
  lists:join($,, Parts).

-spec format_labels(#{binary() := binary()}) -> iodata().
format_labels(Labels) ->
  Parts = [format_label(K, V) || {K, V} <- maps:to_list(Labels)],
  lists:join($,, Parts).

-spec format_label(binary(), binary()) -> iodata().
format_label(Key, Value) ->
  [Key, $=, Value].

-spec format_expressions([expression()]) -> iodata().
format_expressions(Expressions) ->
  Parts = lists:map(fun format_expression/1, Expressions),
  lists:join($,, Parts).

-spec format_expression(expression()) -> iodata().
format_expression(#{key := Key, operator := <<"Exists">>}) ->
  Key;
format_expression(#{key := Key, operator := <<"DoesNotExist">>}) ->
  [$!, Key];
format_expression(Expression = #{key := Key, operator := <<"In">>}) ->
  Values = maps:get(values, Expression, []),
  [Key, " in ", format_values(Values)];
format_expression(Expression = #{key := Key, operator := <<"NotIn">>}) ->
  Values = maps:get(values, Expression, []),
  [Key, " notin ", format_values(Values)].

-spec format_values([binary()]) -> iodata().
format_values(Values) ->
  [$(, lists:join($,, Values), $)].
