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

-module(k8s_units).

-export([parse_cpu/1, parse_memory/1]).

-export_type([error_reason/0]).

-type error_reason() ::
        empty_string
      | invalid_format
      | {invalid_value, binary()}
      | {invalid_unit, binary()}.

-spec parse_cpu(binary()) -> {ok, float()} | {error, error_reason()}.
parse_cpu(<<>>) ->
  {error, empty_string};
parse_cpu(String) ->
  Options = [{capture, all_but_first, binary}],
  case re:run(String, "^([0-9\.]+)([A-Za-z]+)?$", Options) of
    {match, [ValueString]} ->
      parse_cpu_value(ValueString, 1.0);
    {match, [ValueString, <<"m">>]} ->
      parse_cpu_value(ValueString, 1.0e-3);
    {match, [ValueString, <<"n">>]} ->
      parse_cpu_value(ValueString, 1.0e-9);
    {match, [_ValueString, Unit]} ->
      {error, {invalid_unit, Unit}};
    nomatch ->
      {error, invalid_format}
  end.

-spec parse_cpu_value(binary(), float()) ->
        {ok, float()} | {error, error_reason()}.
parse_cpu_value(ValueString, Unit) ->
  try
    Value =
      case string:find(ValueString, <<".">>) of
        nomatch ->
          erlang:binary_to_integer(ValueString) * 1.0;
        _ ->
          erlang:binary_to_float(ValueString)
      end,
    {ok, Value * Unit}
  catch
    error:_ ->
      {error, {invalid_value, ValueString}}
  end.

-spec parse_memory(binary()) ->
        {ok, integer()} | {error, error_reason()}.
parse_memory(<<>>) ->
  {error, empty_string};
parse_memory(String) ->
  Options = [{capture, all_but_first, binary}],
  case re:run(String, "^([0-9\.]+)([A-Za-z]+)?$", Options) of
    {match, [ValueString]} ->
      parse_memory_value(ValueString, 1);
    {match, [ValueString, <<"k">>]} ->
      parse_memory_value(ValueString, 1_000);
    {match, [ValueString, <<"M">>]} ->
      parse_memory_value(ValueString, 1_000_000);
    {match, [ValueString, <<"G">>]} ->
      parse_memory_value(ValueString, 1_000_000_000);
    {match, [ValueString, <<"T">>]} ->
      parse_memory_value(ValueString, 1_000_000_000_000);
    {match, [ValueString, <<"P">>]} ->
      parse_memory_value(ValueString, 1_000_000_000_000_000);
    {match, [ValueString, <<"E">>]} ->
      parse_memory_value(ValueString, 1_000_000_000_000_000_000);
    {match, [ValueString, <<"Ki">>]} ->
      parse_memory_value(ValueString, 1_024);
    {match, [ValueString, <<"Mi">>]} ->
      parse_memory_value(ValueString, 1_048_576);
    {match, [ValueString, <<"Gi">>]} ->
      parse_memory_value(ValueString, 1_073_741_824);
    {match, [ValueString, <<"Ti">>]} ->
      parse_memory_value(ValueString, 1_099_511_627_776);
    {match, [ValueString, <<"Pi">>]} ->
      parse_memory_value(ValueString, 1_125_899_906_842_624);
    {match, [ValueString, <<"Ei">>]} ->
      parse_memory_value(ValueString, 1_152_921_504_606_846_976);
    {match, [_ValueString, Unit]} ->
      {error, {invalid_unit, Unit}};
    nomatch ->
      {error, invalid_format}
  end.

-spec parse_memory_value(binary(), integer()) ->
        {ok, integer()} | {error, error_reason()}.
parse_memory_value(ValueString, Unit) ->
  try
    Value =
      case string:find(ValueString, <<".">>) of
        nomatch ->
          erlang:binary_to_integer(ValueString)*1.0;
        _ ->
          erlang:binary_to_float(ValueString)
      end,
    {ok, erlang:ceil(Value * Unit)}
  catch
    error:_ ->
      {error, {invalid_value, ValueString}}
  end.
