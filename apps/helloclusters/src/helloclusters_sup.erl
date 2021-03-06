%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2014, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2014 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(helloclusters_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    HelloClustersSrv = {helloclusters_srv,
                        {helloclusters_srv, start_link, []},
                        Restart, Shutdown, worker,
                        [helloclusters_srv]},

    ServerSup = {server_sup,
                        {server_sup, start_link, []},
                        Restart, Shutdown, supervisor,
                        [server_sup]},

    {ok, {SupFlags, [HelloClustersSrv, ServerSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

