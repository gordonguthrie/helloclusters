%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2014, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2014 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(helloclusters_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%% To start manually from the shell
-export([
         start/0,
         get_port/0
        ]).

-define(ALLHOSTS, '_').
-define(ALLPATHS, '_').
-define(ASSETS, "/_assets/[...]").

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(helloclusters),

    {ok, NoOfProcesses} = application:get_env(helloclusters, no_of_processes),

    Servers = get_servers(NoOfProcesses, []),
    [supervisor:start_child(server_sup, X) || X <- Servers],
    ok.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ok = initialise_cowboy(),
    case helloclusters_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
initialise_cowboy() ->

    Port = get_port(),

    AssetDirective = get_asset_directive(),

    D = [
         {?ALLHOSTS, [
                      AssetDirective,
                      {?ALLPATHS, web_page_handler, []}]}
        ],

    CDispatch = cowboy_router:compile(D),

    {ok, _PID} = cowboy:start_http(web_page_listener, 100,
                                   [{port, Port}],
                                   [{env, [{dispatch, CDispatch}]}]),
    ok.

get_asset_directive() ->
    Dir       = {directory, helloclusters_utils:get_www_root() ++ "_assets/"},
    Mimetypes = {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
    {?ASSETS,   cowboy_static, [Dir, Mimetypes]}.


get_port() ->
    {ok, IsProd} = application:get_env(helloclusters, is_prod),
    get_p2(IsProd).

get_p2(true) ->
    {ok, Port} = application:get_env(helloclusters, prod_port),
    Port;
get_p2(false) ->
    {ok, Ports} = application:get_env(helloclusters, dev_ports),
    [Node | _Rest] = string:tokens(atom_to_list(node()), "@"),
    {Node, Port} = lists:keyfind(Node, 1, Ports),
    Port.

get_servers(0, Acc) ->
    lists:reverse(Acc);
get_servers(N, Acc) when is_integer(N) andalso N > 0 ->
    ServerName = list_to_atom("server_" ++ integer_to_list(N)),
    NewAcc = {integer_to_list(N),
              {server, start_link, [ServerName]},
              permanent, 2000, worker,
              [server]},
    get_servers(N - 1, [NewAcc | Acc]).
