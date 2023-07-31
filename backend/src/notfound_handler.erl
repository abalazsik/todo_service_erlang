-module(notfound_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(404, #{
        <<"content-type">> => <<"text/html">>
    }, <<"<html><body>404<body></html>">>, Req0),
    {ok, Req, State}.