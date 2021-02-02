-module(k8s_openapi).

-export([rename_model/1]).

-spec rename_model(binary()) -> binary().
rename_model(<<"io.k8s.api.", Name/binary>>) ->
  Name;
rename_model(<<"io.k8s.apiextensions-apiserver.pkg.apis.", Name/binary>>) ->
  Name;
rename_model(<<"io.k8s.apimachinery.pkg.", Name/binary>>) ->
  <<"apimachinery.", Name/binary>>;
rename_model(<<"io.k8s.kube-aggregator.pkg.apis.", Name/binary>>) ->
  Name;
rename_model(Name) ->
  Name.
