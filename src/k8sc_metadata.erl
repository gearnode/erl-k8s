-module(k8sc_metadata).

-export([jsv_definition/0]).

-export_type([metadata/0]).

-type metadata() ::
        #{name => binary(),
          annotations => #{binary() := binary()},
          labels => #{binary() := binary()},
          resource_version => string()}.

-spec jsv_definition() -> jsv:definition().
jsv_definition() ->
  {object,
   #{members =>
       #{name => string,
         annotations => {object, #{value => string}},
         labels => {object, #{value => string}},
         resource_version => string},
     required =>
       []}}.
