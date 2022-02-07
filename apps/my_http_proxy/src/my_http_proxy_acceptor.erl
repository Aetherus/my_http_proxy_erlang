-module(my_http_proxy_acceptor).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3, handle_continue/2]).

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
  process_flag(trap_exit, true),
  {ok, [], {continue, {accept, Opts}}}.

handle_continue({accept, Opts}, _) ->
  ServerSocket = my_http_proxy_server:get_server_socket(),
  {ok, DownstreamSocket} = gen_tcp:accept(ServerSocket, infinity),
  UpstreamProxy = proplists:get_value(upstream_proxy, Opts),
  {ok, Tunnel} = my_http_proxy_tunnels_supervisor:start_child(DownstreamSocket, UpstreamProxy),
  ok = gen_tcp:controlling_process(DownstreamSocket, Tunnel),
  {noreply, ServerSocket, {continue, {accept, Opts}}}.

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

handle_call(Msg, From, State) ->
  {stop, {unexpected_call, Msg, From}, State}.
