%%%-------------------------------------------------------------------
%% @doc backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(backend_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Config = maps:from_list(application:get_all_env(backend)),
    ConnectionStr = maps:get(connectionString, Config),
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [
        #{
            id => database,
            start => {database, start, [ConnectionStr]},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [database]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
