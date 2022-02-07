%%%-------------------------------------------------------------------
%% @doc my_http_proxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(my_http_proxy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      #{
        id => my_http_proxy_tunnels_supervisor,
        start => {my_http_proxy_tunnels_supervisor, start_link, []},
        type => supervisor
      },
      #{
        id => my_http_proxy_acceptor,
        start => {my_http_proxy_acceptor, start_link, [[]]},
        type => worker
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
