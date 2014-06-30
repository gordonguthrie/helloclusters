%%% @author    Gordon Guthrie
%%% @copyright (C) 2014, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 June 2013 by gordon@vixo.com

-module(helloclusters_utils).

-export([
         get_www_root/0
        ]).

get_www_root() ->
{ok, Root} = file:get_cwd(),
    Root ++ "/" ++
        filename:dirname(code:where_is_file("helloclusters.app")) ++
        "/../../../var/docroot/".

