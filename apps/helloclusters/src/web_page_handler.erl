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

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"GET">>  -> handle_get(Req,  State);
        <<"POST">> -> handle_post(Req, State)
    end.

handle_get(Req, State) ->

    PageHeader = make_pageheader("Cluster Controls",
                                 "set 'em to the heart of the sun"),

    NodesTab = make_tab("Nodes", active),
    ProcessesTab = make_tab("Processes", inactive),
    ProcessTab = make_tab("Process", inactive),

    NodesTabBody = make_tabbody("Nodes", io_lib:format("~p", [nodes()]), active),
    ProcessesTabBody = make_tabbody("Processes", "erk", inactive),
    ProcessTabBody = make_tabbody("Process", "berk", inactive),

    Tabs = make_tabs([
                      NodesTab,
                      ProcessesTab,
                      ProcessTab
                     ]),

    TabsBody = make_tabs_body([
                               NodesTabBody,
                               ProcessesTabBody,
                               ProcessTabBody
                              ]),

    Body = [
            PageHeader,
            Tabs,
            TabsBody
           ],

    HTML = make_html([make_head(get_js(), get_css()), make_body(Body)]),

    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

handle_post(_Req, _State) ->
    "ok".

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

make_head(Js, CSS)  when is_list(Js) andalso is_list(CSS) ->
    lists:flatten("<head>"
                  ++ Js
                  ++ CSS
                  ++ "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
              ++ "<head>").

make_body(Body) when is_list(Body) ->
    lists:flatten("<body>"
                  ++ "<div class=\"container\">"
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
    "<script src=\"_assets/jquery/js/jquery-1.10.2.min.js\"></script>" ++
    "<script src=\"_assets/bootstrap/js/bootstrap.js\"></script>".

get_css() ->
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"_assets/bootstrap/css/bootstrap.css\">".

make_tabs(Body) when is_list(Body) ->
    "<!-- Tabs -->" ++
    "<ul class=\"nav nav-tabs\" role = \"tablist\">" ++ Body ++ "</ul>".

make_tab(Title, active) when is_list(Title) ->
    "<li class=\"active\"><a href=\"#" ++ string:to_lower(Title) ++
        "\" role=\"tab\" data-toggle=\"tab\">" ++ Title ++ "</a></li>";
make_tab(Title, inactive) when is_list(Title) ->
    "<li><a href=\"#" ++ string:to_lower(Title) ++
        "\" role=\"tab\" data-toggle=\"tab\">" ++
        Title ++ "</a></li>".

make_tabbody(Title, Body, active)
  when is_list(Title)
       andalso is_list(Body) ->
    "<div class=\"tab-pane active\" id=\"" ++ string:to_lower(Title) ++ "\">" ++
        Body ++ "</div>";
make_tabbody(Title, Body, inactive)
  when is_list(Title)
       andalso is_list(Body) ->
    "<div class=\"tab-pane\" id=\"" ++ string:to_lower(Title) ++ "\">" ++
        Body ++ "</div>".

make_tabs_body(List) when is_list(List) ->
    "<!-- Tab panes -->" ++
    "<div class=\"tab-content\">" ++ List ++ "</div>".

make_pageheader(Headline, Strapline) when is_list(Headline) andalso
                                          is_list(Strapline) ->
    "<div class=\"pageheader\">" ++
        "<h1>" ++ Headline ++ "</h1>" ++
        "<small>" ++ Strapline ++ "</small>" ++
        "</div>".
