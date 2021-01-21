-module(k8s_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = [#{id => resource_registry,
                start => {k8s_resource_registry, start_link, []}}],
  Flags = #{strategy => one_for_one,
            intensity => 1,
            period => 5},
  {ok, {Flags, Children}}.
