%% Copyright (c) 2021 Exograd SAS.
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

-module(k8s_external).

-export([generate_openapi_modules/0]).

-spec generate_openapi_modules() -> no_return().
generate_openapi_modules() ->
  {ok, _} = application:ensure_all_started(openapi),
  SpecPath = filename:join("priv", "kubernetes-openapi-specification.json"),
  OutputDir = "src",
  Options = #{module_prefix => <<"k8s_">>,
              rename_model => fun k8s_openapi:rename_model/1},
  case openapi:generate(SpecPath, OutputDir, Options) of
    ok ->
      halt(0);
    {error, Reason} ->
      io:format(standard_error, "cannot generate modules from ~ts:~n~tp~n",
                [SpecPath, Reason]),
      halt(1)
  end.
