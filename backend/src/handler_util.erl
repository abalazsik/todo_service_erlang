-module(handler_util).

%% @doc useful helper functions

-export([
        parseIntegerQueryParam/2,
        parseIntegerQueryParam/3,
        parseBooleanQueryParam/3,
        getJsonBody/1,
        prepareError/3,
        getRange/1,
        sendJsonResponse/3
    ]).

-spec parseIntegerQueryParam(cowboy_req:req(), Param :: binary(), Default :: integer()) -> any().

parseIntegerQueryParam(Req, Param, Default) ->
    case proplists:get_value(Param, cowboy_req:parse_qs(Req)) of
        Value when is_binary(Value) ->
            erlang:binary_to_integer(Value);
        undefined ->
            Default
    end.

-spec parseIntegerQueryParam(cowboy_req:req(), Param :: binary()) ->
                                undefined | integer().

parseIntegerQueryParam(Req, Param) when is_binary(Param) ->
    case proplists:get_value(Param, cowboy_req:parse_qs(Req)) of
        Value when is_binary(Value) ->
            erlang:binary_to_integer(Value);
        undefined ->
            erlang:throw({error, query_param_not_exists, Param})
    end.

-spec parseBooleanQueryParam(Req :: cowboy_req:req(),
                             Param :: binary(),
                             Default :: boolean()) ->
                                boolean().

parseBooleanQueryParam(Req, Param, Default) when is_binary(Param) ->
    case proplists:get_value(Param, cowboy_req:parse_qs(Req)) of
        <<"true">> ->
            true;
        <<"false">> ->
            false;
        undefined ->
            Default
    end.

-spec getJsonBody(cowboy_req:req()) -> map().

getJsonBody(Req) ->
    {ok, Body, _} = cowboy_req:read_body(Req),
    jsone:decode(Body).


getRange(Req) ->
    case {parseIntegerQueryParam(Req, <<"limit">>, from),
          parseIntegerQueryParam(Req, <<"from">>, limit)}
    of
        {from, limit} ->
            all;
        {Limit, From} ->
            {From, Limit}
    end.

sendJsonResponse(Req, State, Response) -> 
    {ok, cowboy_req:reply(200, #{ <<"content-type">> => <<"application/json">> }, jsone:encode(Response, [native_utf8]), Req), State}.

prepareError(Error, Stacktrace, Req) ->
    logger:debug("Failed to execute the query"),
    erlang:display(Stacktrace),
    cowboy_req:reply(500,
                     #{<<"content-type">> => <<"application/json">>},
                     jsone:encode(#{error =>
                                        erlang:list_to_binary(
                                            io_lib:format("~w", [Error]))},
                                  [native_utf8]),
                     Req).

%TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

parseIntegerQueryParam_test_() ->
    Req = #{
        method => <<"GET">>, path => <<"todo">>, has_read_body => true, qs => <<"id=3">>
    },

    [
        ?_assertEqual(3, parseIntegerQueryParam(Req, <<"id">>)),
        ?_assertThrow({error, query_param_not_exists, <<"id2">>}, parseIntegerQueryParam(Req, <<"id2">>))
    ].

parseIntegerQueryParam_with_default_test_() ->
    Req = #{
        method => <<"GET">>, path => <<"todo">>, has_read_body => true, qs => <<"id=3">>
    },

    [
        ?_assertEqual(3, parseIntegerQueryParam(Req, <<"id">>, 1)),
        ?_assertEqual(1, parseIntegerQueryParam(Req, <<"id2">>, 1))
    ].


parseBooleanQueryParam_test_() ->
    BaseReq = #{
        method => <<"GET">>, path => <<"todo">>, has_read_body => true
    },
    [
        ?_assertEqual(true, parseBooleanQueryParam(maps:merge(BaseReq, #{qs => <<"done=true">>}), <<"done">>, true)),
        ?_assertEqual(false, parseBooleanQueryParam(maps:merge(BaseReq, #{qs => <<"done=false">>}), <<"done">>, true)),
        ?_assertEqual(true, parseBooleanQueryParam(maps:merge(BaseReq, #{qs => <<>>}), <<"done">>, true))
    ].

getRange_test_() ->
    BaseReq = #{
        method => <<"GET">>, path => <<"todo">>, has_read_body => true
    },

    [
        ?_assertEqual({0, from}, getRange(maps:merge(BaseReq, #{qs => <<"from=0">>}))),
        ?_assertEqual({limit, 10}, getRange(maps:merge(BaseReq, #{qs => <<"limit=10">>}))),
        ?_assertEqual({10, 10}, getRange(maps:merge(BaseReq, #{qs => <<"from=10&limit=10">>}))),
        ?_assertEqual(all, getRange(maps:merge(BaseReq, #{qs => <<>>})))
    ].

getJsonBody_test() ->
    meck:new(cowboy_req),
    meck:expect(cowboy_req, read_body, fun(_) -> {ok, <<"{ \"description\": \"this is a description\" }">>, []} end),
    Expected = #{ <<"description">> => <<"this is a description">> },

    try Expected = getJsonBody(#{})
    after
        meck:unload(cowboy_req)
    end.

-endif.