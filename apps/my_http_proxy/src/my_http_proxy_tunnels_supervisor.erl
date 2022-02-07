-module(my_http_proxy_tunnels_supervisor).
-behaviour(supervisor).
-export([start_link/0, start_child/2, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  SupFlags = #{
    strategy => simple_one_for_one
  },
  ChildSpecs = [#{
    id => my_http_proxy_tunnel,
    start => {my_http_proxy_tunnel, start_link, []},
    restart => temporary
  }],
  {ok, {SupFlags, ChildSpecs}}.

start_child(DownstreamSocket, UpstreamProxy) ->
  supervisor:start_child({local, ?MODULE}, [DownstreamSocket, UpstreamProxy]).
