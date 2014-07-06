%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2014, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2014 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(helloclusters_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes = []}).

-include("helloclusters.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{}}.

handle_call(get_nodes, _From, #state{nodes = Nodes} = State) ->
    {reply, Nodes, State};
handle_call(Request, _From, State) ->
    io:format("Call in helloclusters_srv ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({get_config, RemoteNode}, State) ->
    [_, FQDN] = string:tokens(atom_to_list(node()), "@"),
    Config = #config{node = node(),
                    host = FQDN,
                    port = helloclusters_app:get_port()},
    ok = gen_server:cast({?MODULE, RemoteNode}, {config, Config}),
    {noreply, State};
handle_cast({config, Config}, #state{nodes = Nodes} = State) ->
    RemoteNode = Config#config.node,
    NewNodes = lists:keystore(RemoteNode, 1, Nodes, {RemoteNode, Config}),
    {noreply, State#state{nodes = NewNodes}};
handle_cast(Msg, State) ->
    io:format("Handling msg ~p~n", [Msg]),
    {noreply, State}.

handle_info({nodeup, Nodename}, State) ->
    ok = gen_server:cast({?MODULE, Nodename}, {get_config, node()}),
    {noreply, State};
handle_info({nodedown, Nodename}, #state{nodes = Nodes} = State) ->
    NewNodes = lists:keydelete(Nodename, 1, Nodes),
    {noreply, State#state{nodes = NewNodes}};
handle_info(Info, State) ->
    io:format("Info in helloclusters_srv ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = net_kernel:monitor_nodes(false),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
