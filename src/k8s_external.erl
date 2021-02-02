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
      error(Reason)
  end.
