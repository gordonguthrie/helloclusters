-module(server).

-export([
         start_link/1,
         init/1
        ]).

-export([
         loop/1
        ]).

-define(FORMAT, "D M Y H:i:s").

-record(state, {name,
                pid,
                internalstate = none,
                history = [{started, dh_date:format(?FORMAT)}]}).

start_link(Name) ->
    {ok, proc_lib:spawn_link(server, init, [Name])}.

init(Name) ->
    true = erlang:register(Name, self()),
    loop(#state{name = Name, pid = self()}).

loop(State) ->
    #state{name = Name,
           internalstate = InternalState,
           history = History} = State,
    Date = dh_date:format(?FORMAT),
    NewS = receive
               kill ->
                   exit(killed);
               {get_state, From} ->
                   Entry = {looked_at, {by, From}, {'when', Date}},
                   NewHistory = [Entry | History],
                   NewState = State#state{history = NewHistory},
                   From ! {got_state, NewState},
                   NewState;
               Other ->
                   Entry = {unknown_message, {msg, Other}, {'when', Date}},
                   NewHistory = [Entry | History],
                   _NewState = State#state{history = NewHistory}
           end,
    loop(NewS).
