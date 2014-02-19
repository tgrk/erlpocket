%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 7 Apr 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


%% =============================================================================
erlpocket_test_() ->
    {setup,
        fun() ->
                application:set_env(erlpocket, verbose, true),
                erlpocket:start()
        end,
        fun(_) -> erlpocket:stop() end,
        [
         {timeout, 100, {"Unauthorized retrieve call", fun test_unauth_get/0}},
         {timeout, 300, {"Retrieve all items", fun test_get_all/0}},
         {timeout, 100, {"Retrieve unreaded items", fun test_get_unreaded/0}},
         {timeout, 100, {"Retrieve archived items", fun test_get_archive/0}},
         {timeout, 100, {"Retrieve favourite items", fun test_get_favourite/0}},
         {timeout, 100, {"Retrieve items by tag", fun test_get_by_tag/0}},
         %% {timeout, 300, {"Retrieve items stats", fun test_get_stats/0}},
         {timeout, 100, {"Add a new item", fun test_add/0}},
         {timeout, 100, {"Add tags to an item", fun test_tags_add/0}},
         {timeout, 100, {"Delete an existing item", fun test_modify_delete/0}}
        ]
    }.

%% =============================================================================
test_unauth_get() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                     "foo", []),
    ?assertEqual(error, Result).

test_get_all() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                     proplists:get_value(access_token, Keys),
                                     []),
    ?assertEqual(ok, Result).

test_get_unreaded() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                     proplists:get_value(access_token, Keys),
                                     [{state, unread}]),
    ?assertEqual(ok, Result).

test_get_archive() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(
                    proplists:get_value(consumer_key, Keys),
                    proplists:get_value(access_token, Keys),
                    [{state, archive}]
                   ),
    ?assertEqual(ok, Result).

test_get_favourite() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                     proplists:get_value(access_token, Keys),
                                     [{favorite, 1}]),
    ?assertEqual(ok, Result).

test_get_by_tag() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                     proplists:get_value(access_token, Keys),
                                     [{tag, erlang}]),
    ?assertEqual(ok, Result).

%% test_get_stats() ->
%%     Keys = read_api_keys(),
%%     R = erlpocket:stats(
%%           proplists:get_value(consumer_key, Keys),
%%           proplists:get_value(access_token, Keys)
%%          ),
%%     ?debugVal(R),
%%     false.

test_add() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:add(proplists:get_value(consumer_key, Keys),
                                proplists:get_value(access_token, Keys),
                                "http://www.erlang.org/",
                                "test"),
    ?assertEqual(ok, Result).

test_tags_add() ->
    Keys = read_api_keys(),
    ItemId = get_item_id(Keys),
    Result = erlpocket:tags_add(proplists:get_value(consumer_key, Keys),
                                proplists:get_value(access_token, Keys),
                                ItemId,
                                [<<"test">>, <<"test2">>]),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).

test_modify_delete() ->
    Keys = read_api_keys(),
    ItemId = get_item_id(Keys),
    Result = erlpocket:delete(proplists:get_value(consumer_key, Keys),
                              proplists:get_value(access_token, Keys),
                              ItemId),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).


%%%============================================================================
%%% Internal functionality
%%%============================================================================
read_api_keys() ->
    case file:consult("../api.txt") of
        {ok,[Keys]} -> Keys;
        _ -> throw("Unable to read credentials from api.txt file!")
    end.

get_item_id(Keys) ->
    {ok, {PL}} = erlpocket:retrieve(proplists:get_value(consumer_key, Keys),
                                    proplists:get_value(access_token, Keys),
                                    [{search, <<"Erlang Programming Language">>},
                                     {tag,    test}]),
    {[{ItemId, _}]} = proplists:get_value(<<"list">>, PL),
    ItemId.
