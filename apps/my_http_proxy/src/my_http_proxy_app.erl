%%%-------------------------------------------------------------------
%% @doc my_http_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(my_http_proxy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    my_http_proxy_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
