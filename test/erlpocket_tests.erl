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
        fun() -> erlpocket:start() end,
        fun(_) -> erlpocket:stop() end,
        [
         %% {timeout, 100, {"Unauthorized retrieve call", fun test_unauth_get/0}},
         %% {timeout, 300, {"Retrieve all items", fun test_get_all/0}},
         %% {timeout, 100, {"Retrieve unreaded items", fun test_get_unreaded/0}},
         %% {timeout, 100, {"Retrieve archived items", fun test_get_archive/0}},
         %% {timeout, 100, {"Retrieve favourite items", fun test_get_favourite/0}},
         %% {timeout, 100, {"Retrieve items by tag", fun test_get_by_tag/0}},
         %% {timeout, 300, {"Retrieve items stats", fun test_get_stats/0}},
         {timeout, 100, {"Add a new item", fun test_add/0}},
         {timeout, 100, {"Add tags to an item", fun test_tags_add/0}},
         {timeout, 100, {"Remove item's tag", fun test_tags_remove/0}},
         {timeout, 100, {"Replace item's tag", fun test_tags_replace/0}},
         {timeout, 100, {"Rename item's tag", fun test_tags_rename/0}},
         {timeout, 100, {"Remove item's tags", fun test_modify_tags_clear/0}},
         {timeout, 100, {"Delete an existing item", fun test_modify_delete/0}}
        ]
    }.

%% =============================================================================
test_unauth_get() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                     "foo", []),
    ?assertEqual(error, Result).

test_get_all() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                     get_val(access_token, Keys),
                                     []),
    ?assertEqual(ok, Result).

test_get_unreaded() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                     get_val(access_token, Keys),
                                     [{state, unread}]),
    ?assertEqual(ok, Result).

test_get_archive() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(
                    get_val(consumer_key, Keys),
                    get_val(access_token, Keys),
                    [{state, archive}]
                   ),
    ?assertEqual(ok, Result).

test_get_favourite() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                     get_val(access_token, Keys),
                                     [{favorite, 1}]),
    ?assertEqual(ok, Result).

test_get_by_tag() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                     get_val(access_token, Keys),
                                     [{tag, erlang}]),
    ?assertEqual(ok, Result).

%% test_get_stats() ->
%%     Keys = read_api_keys(),
%%     R = erlpocket:stats(
%%           get_val(consumer_key, Keys),
%%           get_val(access_token, Keys)
%%          ),
%%     ?debugVal(R),
%%     false.

test_add() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:add(get_val(consumer_key, Keys),
                                get_val(access_token, Keys),
                                "http://www.erlang.org/",
                                "test"),
    ?assertEqual(ok, Result).

test_tags_add() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:tags_add(get_val(consumer_key, Keys),
                                get_val(access_token, Keys),
                                ItemId,
                                [<<"test">>, <<"test2">>]),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).

test_tags_remove() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:tags_remove(get_val(consumer_key, Keys),
                                  get_val(access_token, Keys),
                                  ItemId, [<<"test2">>]),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertEqual([],  search_item(Keys, <<"test2">>)).

test_tags_replace() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:tags_replace(get_val(consumer_key, Keys),
                                    get_val(access_token, Keys),
                                    ItemId, [<<"test3">>]),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertNotEqual([],  search_item(Keys, <<"test3">>)).

test_tags_rename() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:tags_rename(get_val(consumer_key, Keys),
                                   get_val(access_token, Keys),
                                   ItemId, <<"test3">>, <<"test4">>),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertNotEqual([],  search_item(Keys, <<"test4">>)).

test_modify_tags_clear() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:tags_clear(get_val(consumer_key, Keys),
                                  get_val(access_token, Keys),
                                  ItemId),
    ?assertEqual({ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertEqual([],  search_item(Keys, <<"test">>)).

test_modify_delete() ->
    Keys = read_api_keys(),
    {ItemId, _} = search_item(Keys),
    Result = erlpocket:delete(get_val(consumer_key, Keys),
                              get_val(access_token, Keys),
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

%%NOTE: this is only dev helper
delete_items(Keys, Ids) ->
    lists:foreach(fun (Id) ->
                          erlpocket:delete(get_val(consumer_key, Keys),
                                           get_val(access_token, Keys),
                                           Id)
                  end, Ids).

search_item(Keys) ->
    {ok, {PL}} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                   get_val(access_token, Keys),
                                   [{search, <<"Erlang Programming Language">>}]
                                   ),
    case get_val(<<"list">>, PL)  of
        []                   -> [];
        {[{ItemId, {Item}}]} -> {ItemId, Item}
    end.

search_item(Keys, Tag) ->
    {ok, {PL}} = erlpocket:retrieve(get_val(consumer_key, Keys),
                            get_val(access_token, Keys),
                            [{search, <<"Erlang Programming Language">>},
                             {tag, Tag}]),
    case get_val(<<"list">>, PL)  of
        []                   -> [];
        {[{ItemId, {Item}}]} -> {ItemId, Item}
    end.

get_val(Key, PL) ->
    proplists:get_value(Key, PL).
