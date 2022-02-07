-module(my_http_proxy_acceptor).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3, handle_continue/2, terminate/2]).

start_link(Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
  erlang:display(Opts),
  IP = proplists:get_value(ip, Opts, {127, 0, 0, 1}),
  Port = proplists:get_value(port, Opts, 1080),
  case listen(IP, Port) of
    {ok, ServerSocket} ->
      {ok, ServerSocket, {continue, {accept, Opts}}};
    Error ->
      Error
  end.

handle_continue({accept, Opts}, ServerSocket) ->
  {ok, DownstreamSocket} = gen_tcp:accept(ServerSocket, infinity),
  UpstreamProxy = proplists:get_value(upstream_proxy, Opts),
  {ok, Tunnel} = my_http_proxy_tunnels_supervisor:start_child(DownstreamSocket, UpstreamProxy),
  ok = gen_tcp:controlling_process(DownstreamSocket, Tunnel),
  {noreply, ServerSocket, {continue, {accept, Opts}}}.

terminate(_, ServerSocket) ->
  gen_tcp:shutdown(ServerSocket, read_write).

listen(IP, Port) ->
  gen_tcp:listen(Port, [
    {mode, binary},
    {active, false},
    {backlog, 5},
    {keepalive, true},
    {ip, IP}
  ]).

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

handle_call(Msg, From, State) ->
  {stop, {unexpected_call, Msg, From}, State}.

