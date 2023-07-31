-module(todo_service).
-export([create/1, list/2, update/1]).

create(#{<<"description">> := Description, <<"size">> := Size}) when is_binary(Description) andalso is_binary(Size) ->
    Command = io_lib:format("INSERT INTO todo (description, size_) VALUES ('~ts', '~ts') RETURNING id",
        [
            database_util:sqlEscape(Description),
            database_util:sqlEscape(Size)
        ]),
    [{IdAsBinary}] = database:run(Command),
    erlang:list_to_integer(IdAsBinary).

list(Done, Page) ->
    Command = "SELECT id, creation_date, description, done, size_ FROM todo " ++ where(Done) ++ "ORDER BY creation_date DESC " ++ database_util:page(Page),
    database:run(Command).

where(all) -> [];
where(true) -> "WHERE done = TRUE ";
where(false) -> "WHERE done = FALSE ".

update(#{<<"description">> := Description, <<"size">> := Size, <<"done">> := Done, <<"id">> := Id}) ->
    Command = io_lib:format("UPDATE todo SET description = '~ts', size_ = '~ts', done = ~w WHERE id = ~w",
        [
            database_util:sqlEscape(Description),
            database_util:sqlEscape(size_(Size)),
            Done,
            Id
        ]),
    1 == database:run(Command).

%%========INTERNAL FUNCTIONS========

size_(Size) when is_binary(Size) ->
    size_(erlang:binary_to_list(Size));
size_(Size) when Size == "s" orelse Size == "m" orelse Size == "l"  orelse Size == "h" ->
     Size;
size_(Other) -> erlang:throw({invalid_size, Other}).


%TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

size_test_() -> [
    ?_assertEqual("s", size_("s")),
    ?_assertEqual("l", size_(<<"l">>)),
    ?_assertThrow({invalid_size, "o"}, size_("o"))
].

list_all_test() ->
    meck:new(database),
    meck:expect(database, run, fun("SELECT id, creation_date, description, done, size_ FROM todo ORDER BY creation_date DESC ") -> ok end),

    try ok = list(all, all) 
    after
        meck:unload(database)
    end.

list_all_done_test() ->
    meck:new(database),
    meck:expect(database, run, fun("SELECT id, creation_date, description, done, size_ FROM todo WHERE done = TRUE ORDER BY creation_date DESC ") -> ok end),

    try ok = list(true, all) 
    after
        meck:unload(database)
    end.

list_all_not_done_test() ->
    meck:new(database),
    meck:expect(database, run, fun("SELECT id, creation_date, description, done, size_ FROM todo WHERE done = FALSE ORDER BY creation_date DESC ") -> ok end),

    try ok = list(false, all) 
    after
        meck:unload(database)
    end.

update_test() ->
    meck:new(database),
    meck:expect(database, run,
        fun(Command) -> 
            "UPDATE todo SET description = 'description', size_ = 'l', done = true WHERE id = 11" == unicode:characters_to_list(Command), 1 end),

    try true = update(#{ <<"description">> => <<"description">>, <<"size">> => <<"l">>, <<"done">> => true, <<"id">> => 11}) 
    after
        meck:unload(database)
    end.

create_test() ->
    meck:new(database),
    meck:expect(database, run,
        fun(Command) -> 
            "INSERT INTO todo (description, size_) VALUES ('description', 'l') RETURNING id" == unicode:characters_to_list(Command),
            [{"3"}]
        end),

    try 3 = create(#{ <<"description">> => <<"description">>, <<"size">> => <<"l">>}) 
    after
        meck:unload(database)
    end.

-endif.
