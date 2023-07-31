-module(database).

%%behaviour functions implemented by this module
-export([init/1, start/1, terminate/2, handle_call/3]).

-export([run/1]).

-behaviour(gen_server).

-record(int_state, {ref :: pid() }).

init(ConnectionStr)->
	{ok, Ref} = odbc:connect(ConnectionStr, []),
	{ok, #int_state{ref = Ref }}.

start(ConnectionStr) when is_list(ConnectionStr) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ConnectionStr, []).


terminate(_, State) ->
	odbc:disconnect(State#int_state.ref).

handle_call(Request, _, State) ->
	Ref = State#int_state.ref,
	case Request of
		{command, Command} ->
			Result = odbc:sql_query(Ref, Command),
			{reply, Result, State};
		_ ->
			{reply, unknown_command}
    end.

run(Command) when is_list(Command) ->
	logger:debug("~ts~n",[Command]),
	result(gen_server:call(?MODULE,{command, Command})).

result({selected, Columns, Rows}) when is_list(Columns) andalso is_list(Rows) -> Rows;
result({updated, N}) -> N;
result({ok, 1, Columns, [{Id}]}) when is_list(Columns) -> erlang:binary_to_integer(Id);
result(List) when is_list(List) ->
	[result(X) || X <- List];
result(Result) -> Result.
