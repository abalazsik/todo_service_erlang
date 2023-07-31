-module(database_util).
-export([sqlEscape/1, page/1]).

sqlEscape(Text) when is_list(Text) ->
    sqlEscape(Text,[]);
sqlEscape(Text) when is_binary(Text) ->
    sqlEscape(erlang:binary_to_list(Text),[]).

page(all) -> [];
page({limit, Limit}) when is_integer(Limit) andalso Limit > 0 -> "LIMIT " ++ erlang:integer_to_list(Limit);
page({From, from}) when is_integer(From) andalso From >= 0 -> "OFFSET " ++ erlang:integer_to_list(From);
page({From, Limit}) when is_integer(Limit) andalso Limit >= 1 andalso is_integer(From) andalso From >= 0 ->
    "OFFSET " ++ erlang:integer_to_list(From) ++ " LIMIT " ++ erlang:integer_to_list(Limit);
page(Page) -> erlang:throw({invalid_page, Page}).

%%========INTERNAL FUNCTIONS========

sqlEscape([], Result) ->
    lists:reverse(Result);
sqlEscape([$' | Rest], Result) ->
    sqlEscape(Rest,[$',$'] ++ Result);
sqlEscape([C | Rest], Result) ->
    sqlEscape(Rest,[C] ++ Result).

%TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sqlEscape_test_() -> [
    ?_assertEqual("'' drop table todo", sqlEscape("' drop table todo")),
    ?_assertEqual("don''t", sqlEscape(<<"don't">>))
].

page_test_() -> [
    ?_assertEqual([], page(all)),
    ?_assertEqual("LIMIT 10", page({limit, 10})),
    ?_assertEqual("OFFSET 10", page({10, from})),
    ?_assertEqual("OFFSET 10 LIMIT 10", page({10, 10})),
    ?_assertThrow({invalid_page, book}, page(book))
].

-endif.
