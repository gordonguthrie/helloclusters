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
         start/0
        ]).

-define(ALLHOSTS, '_').
-define(ALLPATHS, '_').
-define(ASSETS, "/_assets/[...]").

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(helloclusters).

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

    {ok, IsProd} = application:get_env(helloclusters, is_prod),

    Port = get_port(IsProd),

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

get_port(true) ->
    {ok, Port} = application:get_env(helloclusters, prod_port),
    Port;
get_port(false) ->
    {ok, Ports} = application:get_env(helloclusters, dev_ports),
    [Node | _Rest] = string:tokens(atom_to_list(node()), "@"),
    {Node, Port} = lists:keyfind(Node, 1, Ports),
    Port.
