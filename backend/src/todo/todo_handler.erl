-module(todo_handler).
-export([init/2]).

init(Req, State) ->
    try cowboy_req:method(Req) of
        <<"GET">> ->
            Range = handler_util:getRange(Req),
            DoneSearch = handler_util:parseBooleanQueryParam(Req, <<"done">>, all),
            Todos = todo_service:list(DoneSearch, Range),
            handler_util:sendJsonResponse(Req, State, mapTodo(Todos));
        <<"PUT">> ->
            Todo = handler_util:getJsonBody(Req),
            handler_util:sendJsonResponse(Req, State, todo_service:create(Todo));
        <<"POST">> ->
            Todo = handler_util:getJsonBody(Req),
            handler_util:sendJsonResponse(Req, State, todo_service:update(Todo));
        <<"OPTIONS">> ->
            {true, Req, State};
        _ ->
            {stop, Req, State}
    catch Error:_:Stacktrace ->
        {true, handler_util:prepareError(Error, Stacktrace, Req), State} 
    end.

mapTodo([]) -> [];
mapTodo([Todo | Rest]) ->
    [mapTodo(Todo)] ++ mapTodo(Rest);
mapTodo({IdAsString, CreationDate, Description, Done, Size}) ->
    #{
        id => erlang:list_to_integer(IdAsString),
        creation_date => iso8601:format(CreationDate),
        description => erlang:list_to_binary(Description),
        done => boolean(Done),
        size => erlang:list_to_binary(Size)
    }.

boolean("1") -> true;
boolean("0") -> false.

%TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

mapping_test_() ->
    Expected = [#{
        id => 1,
        creation_date => <<"2022-08-09T08:30:00Z">>,
        description => <<"Don't fuck up, idiot!">>,
        done => false,
        size => <<"h">>
    },
    #{
        id => 2,
        creation_date => <<"2022-08-09T08:30:00Z">>,
        description => <<"description">>,
        done => true,
        size => <<"s">>
    }],

    Result = mapTodo(
        [
            {"1", {{2022, 8, 9}, {8, 30, 00}}, "Don't fuck up, idiot!", "0", "h"},
            {"2", {{2022, 8, 9}, {8, 30, 00}}, "description", "1", "s"}
        ]),

    [
        ?_assertEqual(Expected, Result)
    ].

get_all_test() ->
    BaseReq = #{
        path => <<"todo">>, has_read_body => true
    },

    meck:new(todo_service),
    meck:expect(todo_service, list, fun(all, all) -> [] end),

    meck:new(cowboy_req),
    meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
    meck:expect(cowboy_req, parse_qs, fun(_) -> [] end),
    meck:expect(cowboy_req, reply, fun(200, _, _, _) -> ok end),

    try init(BaseReq, [])
    after
        meck:unload(todo_service),
        meck:unload(cowboy_req)
    end.

%%writing the remaining tests left as an exercise for the reader

-endif.