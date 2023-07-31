%%%-------------------------------------------------------------------
%% @doc backend public API
%% @end
%%%-------------------------------------------------------------------

-module(backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    MainDef = cowboy_router:compile([{'_',[
		{"/todo", todo_handler, #{}},
        {"/[...]", notfound_handler, #{}}
    ]}]),
    
    {ok, _} = cowboy:start_clear(main,
    [
        {port, 8080}
    ],

    #{
        env => #{ dispatch => MainDef },
        middlewares => [cowboy_router, cowboy_handler]
    }),
    backend_sup:start_link().

stop(_State) ->
    ok.

