-module(my_http_proxy_acceptors_supervisor).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(Count, AcceptorOpts) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {Count, AcceptorOpts}).

init({Count, AcceptorOpts}) ->
  Nums = lists:seq(1, Count),
  SupFlags = #{strategy => one_for_one},
  ChildSpecs = lists:map(fun (N) ->
    #{
      id => {?MODULE, N},
      start => {my_http_proxy_acceptor, start_link, [AcceptorOpts]},
      restart => permanent,
      shutdown => brutal_kill
    }  
  end, Nums),
  {ok, {SupFlags, ChildSpecs}}.
