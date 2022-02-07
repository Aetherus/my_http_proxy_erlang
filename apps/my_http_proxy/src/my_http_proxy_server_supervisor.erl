-module(my_http_proxy_server_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(ServerOpts) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ServerOpts).

init(ServerOpts) ->
  SupFlags = #{strategy => rest_for_one},
  ChildSpecs = [
    #{
      id => my_http_proxy_server,
      start => {my_http_proxy_server, start_link, [ServerOpts]},
      type => worker
    },
    #{
      id => my_http_proxy_acceptors_supervisor,
      start => {my_http_proxy_acceptors_supervisor, start_link, [10, []]},
      type => worker
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.