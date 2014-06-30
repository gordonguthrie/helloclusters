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

    Body = [
            make_h2("Nodes"),
            io_lib:format("~p", [nodes()])
           ],
    HTML = make_html([make_head([], get_css()), make_body(Body)]),

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
                  ++ "<head>").

make_body(Body) when is_list(Body) ->
    lists:flatten("<body>"
                  ++ Body
                  ++ "</body>").

make_html(HTML) when is_list(HTML) ->
    lists:flatten("<html>"
                  ++ HTML
                  ++ "</html>").

make_h2(Heading) when is_list(Heading) ->
    "<h2>" ++
        Heading ++
        "</h2>".

get_css() ->
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"_assets/bootstrap/css/bootstrap-responsive.css\">".
