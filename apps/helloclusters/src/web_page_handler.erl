%%% @author    Gordon Guthrie
%%% @copyright (C) 2014, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 26 June 2014 by gordon@vixo.com

-module(web_page_handler).

-behaviour(cowboy_http_handler).

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

-include("helloclusters.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    io:format("Handling ~p~n", [Method]),
    case Method of
        <<"GET">>  -> handle_get(Req,  State);
        <<"POST">> -> handle_post(Req, State)
    end.

handle_get(Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    case Path of
        <<"/server/", Server/binary>> ->
            Server2 = binary_to_list(Server),
            case is_valid(Server2) of
                true ->
                    server_page(Server2, Req, State);
                false ->
                    front_page(Req, State)
                end;
        _ ->
            front_page(Req, State)
    end.

server_page(Server, Req, State) ->
    list_to_atom(Server) ! {get_state, self()},
    Contents = receive
               {got_state, {state, _Server, _PID, InternalState, History}} ->
                       [
                        "<h3>Internal State</h3>",
                        "<pre>",
                        io_lib:format("~p", [InternalState]),
                        "</pre>",
                        "<h3>History</h3>",
                        "<pre>",
                        io_lib:format("~p~n", [History]),
                        "</pre>"
                       ]
               after
                   5000 ->
                       "<div>Request timed out...</div>"
           end,
    PageHeader = make_pageheader("Cluster Controls",
                                 "set 'em to the heart of the sun"),

    Headline = "<h2>Introspecting <span>" ++ Server ++ "</span></h2>",

    Form = [
            "<h2>Send Message</h2>",
            "<form>",
            "<button type='submit' formmethod='post'>Send Message</button>",
            "<input type='input'></input>",
            "</form>"
           ],

    Body = [
            PageHeader,
            make_line(),
            Headline,
            Contents,
            make_line(),
            Form
           ],

    HTML = make_html([make_head(get_js(), get_css()), make_body(Body)]),

    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

front_page(Req, State) ->
    PageHeader = make_pageheader("Cluster Controls",
                                 "set 'em to the heart of the sun"),

    NodesTab   = make_tab("Nodes", active),
    ServersTab = make_tab("Servers", inactive),

    Nodes     = gen_server:call(helloclusters_srv, get_nodes),
    NodesHTML = make_nodes(Nodes),

    Servers = make_servers(),

    NodesTabBody   = make_tabbody("Nodes", NodesHTML, active),
    ServersTabBody = make_tabbody("Servers", Servers, inactive),

    Tabs = make_tabs([
                      NodesTab,
                      ServersTab
                     ]),

    TabsBody = make_tabs_body([
                               NodesTabBody,
                               ServersTabBody
                              ]),

    Body = [
            PageHeader,
            Tabs,
            TabsBody
           ],

    HTML = make_html([make_head(get_js(), get_css()), make_body(Body)]),

    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

handle_post(Req, State) ->
    io:format("in Post Req is ~p~n", [Req]),
    HTML = "erk",
    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

make_head(Js, CSS)  when is_list(Js) andalso is_list(CSS) ->
    lists:flatten("<head>"
                  ++ Js
                  ++ CSS
                  ++ "<meta name='viewport' content='width=device-width, initial-scale=1'>"
              ++ "<head>").

make_body(Body) when is_list(Body) ->
    lists:flatten("<body>"
                  ++ "<div class='container'>"
                  ++ Body
                  ++ "</div>"
                  ++ "</body>").

make_html(HTML) when is_list(HTML) ->
    lists:flatten("<html>"
                  ++ lists:flatten(HTML)
                  ++ "</html>").

make_h2(Heading) when is_list(Heading) ->
    "<h2>" ++
        Heading ++
        "</h2>".

get_js() ->
    "<script src='/_assets/jquery/js/jquery-1.10.2.min.js'></script>" ++
    "<script src='/_assets/bootstrap/js/bootstrap.js'></script>".

get_css() ->
    "<link rel='stylesheet' type='text/css' href='/_assets/bootstrap/css/bootstrap.css'>".

make_tabs(Body) when is_list(Body) ->
    "<!-- Tabs -->" ++
    "<ul class='nav nav-tabs' role = 'tablist'>" ++ Body ++ "</ul>".

make_tab(Title, active) when is_list(Title) ->
    "<li class='active'><a href='#" ++ string:to_lower(Title) ++
        "' role='tab' data-toggle='tab'>" ++ Title ++ "</a></li>";
make_tab(Title, inactive) when is_list(Title) ->
    "<li><a href='#" ++ string:to_lower(Title) ++
        "' role='tab' data-toggle='tab'>" ++
        Title ++ "</a></li>".

make_tabbody(Title, Body, active)
  when is_list(Title)
       andalso is_list(Body) ->
    "<div class='tab-pane active' id='" ++ string:to_lower(Title) ++ "'>" ++
        Body ++ "</div>";
make_tabbody(Title, Body, inactive)
  when is_list(Title)
       andalso is_list(Body) ->
    "<div class='tab-pane' id='" ++ string:to_lower(Title) ++ "'>" ++
        Body ++ "</div>".

make_tabs_body(List) when is_list(List) ->
    "<!-- Tab panes -->" ++
    "<div class='tab-content'>" ++ List ++ "</div>".

make_pageheader(Headline, Strapline) when is_list(Headline) andalso
                                          is_list(Strapline) ->
    "<div class='pageheader'>" ++
        "<h1><a href='/'>" ++ Headline ++ "</a></h1>" ++
        "<small>" ++ Strapline ++ "</small>" ++
        "</div>".

make_nodes(List) when is_list(List) ->
    make_n2(lists:sort(List), []).

make_n2([], Acc) ->
    lists:reverse(Acc);
make_n2([{N, #config{node = N, host = H, port = P}} | T], Acc) ->
    NewAcc = "<div><a href='http://" ++ H ++ ":" ++ integer_to_list(P) ++ "'"
        ++ ">" ++ atom_to_list(N) ++ "</a></div>",
    make_n2(T, [NewAcc | Acc]).

make_servers() ->
    {ok, NoOfServers} = application:get_env(helloclusters, no_of_processes),
    make_s2(NoOfServers, []).

make_s2(0, Acc) ->
    Acc;
make_s2(N, Acc) when is_integer(N) andalso N > 0 ->
    Name = "server_" ++ integer_to_list(N),
    NewAcc = "<div><a href='/server/" ++ Name ++ "'>" ++
        Name ++ "</a></div>",
    make_s2(N - 1, [NewAcc | Acc]).

is_valid("server_" ++ N) ->
    try
        N2 = list_to_integer(N),
        {ok, NoOfServers} = application:get_env(helloclusters, no_of_processes),
        if
            N2 =< NoOfServers andalso N2 > 0 -> true;
            N2 >  NoOfServers                -> false;
            N2 =< 0                          -> false
        end
    catch
        _ -> false
    end;
is_valid(_) ->
    false.

make_line() ->
    "<div style='display:block;width:100%;border-bottom:1px solid #666'></div>".
