-module(my_http_proxy_tunnel).
-behaviour(gen_server).
-export([start_link/2, init/1, handle_continue/2, handle_info/2, terminate/2, handle_call/3, handle_cast/2]).

start_link(DownstreamSocket, UpstreamProxy) ->
  gen_server:start_link(?MODULE, {DownstreamSocket, UpstreamProxy}, [{name, ?MODULE}]).

init({DownstreamSocket, UpstreamProxy}) ->
  process_flag(trap_exit, true),
  {ok, {DownstreamSocket, undefined}, {continue, {handshake, UpstreamProxy}}}.

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

handle_call(Msg, From, State) ->
  {stop, {unexpected_call, Msg, From}, State}.

handle_continue({handshake, UpstreamProxy}, {DownstreamSocket, undefined}) ->
  {ok, Request} = gen_tcp:recv(DownstreamSocket, 0),
  [RequestLine, _HeadersAndBody] = binary:split(Request, <<"\r\n">>, []),
  {ok, TargetHost, TargetPort, Protocol} = parse_request_line(RequestLine),
  case connect_to_upstream(TargetHost, TargetPort, UpstreamProxy) of
    {error, Reason} ->
      send_handshake_error_response(DownstreamSocket, Reason, Protocol),
      {stop, normal, {DownstreamSocket, undefined}};
    {ok, UpstreamSocket} ->
      ok = send_handshake_ok_response(DownstreamSocket, Protocol),
      ok = inet:setopts(DownstreamSocket, [{active, true}]),
      {noreply, {DownstreamSocket, UpstreamSocket}}
  end.

handle_info({tcp, DownstreamSocket, Data}, {DownstreamSocket, UpstreamSocket} = State) ->
  case UpstreamSocket of
    undefined ->
      {noreply, State};
    _ ->
      gen_tcp:send(UpstreamSocket, Data),
      {noreply, State}
  end;

handle_info({tcp, UpstreamSocket, Data}, {DownstreamSocket, UpstreamSocket} = State) ->
  case DownstreamSocket of
    undefined ->
      {noreply, State};
    _ ->
      gen_tcp:send(DownstreamSocket, Data),
      {noreply, State}
  end;

handle_info({tcp_closed, _}, State) ->
  {stop, normal, State};

handle_info(_, State) ->
  {noreply, State}.

terminate(_, {undefined, undefined}) ->
  ok;

terminate(_, {undefined, UpstreamSocket}) ->
  gen_tcp:shutdown(UpstreamSocket, read_write);

terminate(Msg, {DownstreamSocket, UpstreamSocket}) ->
  gen_tcp:shutdown(DownstreamSocket, read_write),
  terminate(Msg, {undefined, UpstreamSocket}).

parse_request_line(RequestLine) ->
  [<<"CONNECT">>, TargetHostAndPort, Protocol] = binary:split(RequestLine, <<" ">>, [trim, global]),
  [TargetHostBin, TargetPortBin] = binary:split(TargetHostAndPort, <<":">>, [trim]),
  TargetHost = binary:bin_to_list(TargetHostBin),
  TargetPort = erlang:binary_to_integer(TargetPortBin),
  {ok, TargetHost, TargetPort, Protocol}.

connect_to_upstream(TargetHost, TargetPort, undefined) ->
  gen_tcp:connect(TargetHost, TargetPort, [
    {active, true},
    {mode, binary},
    {keepalive, true}
  ]);

connect_to_upstream(TargetHost, TargetPort, UpstreamProxy) ->
  connect_to_upstream_proxy(UpstreamProxy, TargetHost, TargetPort).

connect_to_upstream_proxy(UpstreamProxy, TargetHost, TargetPort) ->
  UpstreamHost = proplists:get_value(host, UpstreamProxy),
  UpstreamPort = proplists:get_value(port, UpstreamProxy),
  {ok, UpstreamSocket} = gen_tcp:connect(UpstreamHost, UpstreamPort, [
    {active, false},
    {mode, binary},
    {keepalive, true}
  ]),
  ok = gen_tcp:send(UpstreamSocket, build_handshake_request(TargetHost, TargetPort)),
  {ok, Response} = gen_tcp:recv(UpstreamSocket, 0),
  ok = validate_handshake_response(Response),
  ok = inet:setopts(UpstreamSocket, [{active, true}]),
  {ok, UpstreamSocket}.

build_handshake_request(TargetHost, TargetPort) ->
  PortString = integer_to_list(TargetPort),
  <<
    "CONNECT ", TargetHost, ":", PortString, " HTTP/1.0\r\n",
    "Host: ", TargetHost, ":", PortString, "\r\n",
    "\r\n"
  >>.

validate_handshake_response(Response) ->
  [<<"HTTP/1.0 200 OK">>, _HeadersAndBody] = binary:split(Response, <<"\r\n">>, []),
  ok.

send_handshake_ok_response(DownstreamSocket, Protocol) ->
  gen_tcp:send(DownstreamSocket, build_handshake_ok_response(Protocol)).

send_handshake_error_response(DownstreamSocket, Reason, Protocol) ->
  gen_tcp:send(DownstreamSocket, build_handshake_error_response(Reason, Protocol)).

build_handshake_ok_response(Protocol) ->
  iolist_to_binary([
    Protocol, " 200 OK\r\n",
    "Connection: close\r\n\r\n"
  ]).

build_handshake_error_response(_Reason, Protocol) ->
  iolist_to_binary([
    Protocol, " 502 Bad Gateway\r\n",
    "Content-Length: 0\r\n",
    "Connection: close\r\n\r\n"
  ]).
