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

-export([parse_cpu_usage/1]).

-export_type([error_reason/0]).

-type error_reason() ::
        empty_string
      | invalid_format
      | {invalid_value, binary()}
      | {invalid_unit, binary()}.

-spec parse_cpu_usage(binary()) -> {ok, float()} | {error, error_reason()}.
parse_cpu_usage(<<>>) ->
  {error, empty_string};
parse_cpu_usage(String) ->
  Options = [{capture, all_but_first, binary}],
  case re:run(String, "^([0-9\.]+)([A-Za-z]+)?$", Options) of
    {match, [ValueString]} ->
      parse_value(ValueString, 1.0);
    {match, [ValueString, <<"m">>]} ->
      parse_value(ValueString, 1.0e-3);
    {match, [ValueString, <<"n">>]} ->
      parse_value(ValueString, 1.0e-9);
    {match, [_ValueString, Unit]} ->
      {error, {invalid_unit, Unit}};
    nomatch ->
      {error, invalid_format}
  end.

-spec parse_value(binary(), float()) ->
        {ok, float()} | {error, error_reason()}.
parse_value(ValueString, Unit) ->
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
