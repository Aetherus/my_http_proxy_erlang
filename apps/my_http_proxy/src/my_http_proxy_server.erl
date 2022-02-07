-module(my_http_proxy_server).
-behaviour(gen_server).
-export([start_link/1, init/1, get_server_socket/0, handle_call/3, handle_cast/2, terminate/2]).

start_link(Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

get_server_socket() ->
  gen_server:call(?MODULE, get_server_socket).

handle_call(get_server_socket, _From, ServerSocket) ->
  {reply, ServerSocket, ServerSocket}.

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

init(Opts) ->
  process_flag(trap_exit, true),
  IP = proplists:get_value(ip, Opts, {127, 0, 0, 1}),
  Port = proplists:get_value(port, Opts, 1080),
  case listen(IP, Port) of
    {ok, ServerSocket} ->
      {ok, ServerSocket};
    Error ->
      Error
  end.

listen(IP, Port) ->
  gen_tcp:listen(Port, [
    {mode, binary},
    {active, false},
    {backlog, 5},
    {keepalive, true},
    {ip, IP}
  ]).

terminate(_Reason, ServerSocket) ->
  gen_tcp:shutdown(ServerSocket, read_write).