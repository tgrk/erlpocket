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
     fun() -> application:ensure_all_started(erlpocket) end,
     fun(_) -> ok end,
     [
      {timeout, 100, {"Custom oAuth test", fun test_oauth/0}},
      {timeout, 100, {"Unauthorized retrieve call", fun test_unauth_get/0}},
      {timeout, 600, {"Retrieve all items", fun test_get_all/0}},
      {timeout, 100, {"Validate params", fun test_validate_params/0}},
      {timeout, 100, {"Invalid params check", fun test_invalid_params_check/0}},
      {timeout, 100, {"Retrieve unreaded items", fun test_get_unreaded/0}},
      {timeout, 100, {"Retrieve archived items", fun test_get_archive/0}},
      {timeout, 100, {"Retrieve favourite items", fun test_get_favourite/0}},
      {timeout, 100, {"Retrieve items by tag", fun test_get_by_tag/0}},
      {timeout, 300, {"Retrieve items stats", fun test_get_stats/0}},
      {timeout, 100, {"Add a new item", fun test_add/0}},
      {timeout, 100, {"Mark/unmark item favorite", fun test_favorite/0}},
      {timeout, 100, {"Archive/readd item", fun test_archive/0}},
      {timeout, 100, {"Add tags to an item", fun test_tags_add/0}},
      {timeout, 100, {"Remove item's tag", fun test_tags_remove/0}},
      {timeout, 100, {"Replace item's tag", fun test_tags_replace/0}},
      {timeout, 100, {"Rename item's tag", fun test_tag_rename/0}},
      {timeout, 100, {"Remove item's tags", fun test_modify_tags_clear/0}},
      {timeout, 100, {"Delete an existing item", fun test_modify_delete/0}}
     ]
    }.

%% =============================================================================
test_oauth() ->
    Keys = read_api_keys(),
    Url = "http://https://github.com/tgrk/erlpocket/",
    {ok, _Headers, [{code, Code}]} =
        erlpocket:request_token(get_val(consumer_key, Keys), Url),

    AuthUrl = erlpocket:get_authorize_url(Code, Url),
    [_, Args] = string:tokens(AuthUrl, "?"),
    [Arg1, Arg2] = string:tokens(Args, "&"),
    ?assertEqual("request_token=" ++ Code, Arg1),
    ?assertEqual("redirect_uri=" ++ Url, Arg2),

    %% since we didn't click on authorization url, final oAuth step should fail
    ?assertEqual(
       {error,{invalid_consumer_key,
            [{"x-error","User rejected code."},
             {"x-error-code","158"},
             {"x-limit-key-limit",[]},
             {"x-limit-key-remaining",[]},
             {"x-limit-key-reset",[]},
             {"x-limit-user-limit",[]},
             {"x-limit-user-remaining",[]},
             {"x-limit-user-reset",[]},
             {"x-source","Pocket"}]}},
       erlpocket:authorize(get_val(consumer_key, Keys), Code)).

test_unauth_get() ->
    Keys = read_api_keys(),
    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys), "foo", []),
    ?assertEqual(error, Result).

test_get_all() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                        get_val(access_token, Keys), []),
    ?assertEqual(ok, Result).

test_validate_params() ->
    ?assertEqual(true, erlpocket:is_valid_param(add, [{url, "http://foobar"}])),
    ?assertEqual(true, erlpocket:is_valid_param(modify, [{action, tags_clear}])),
    ?assertEqual(false, erlpocket:is_valid_query([{url, "http://foobar"}])),
    ?assertEqual(false, erlpocket:is_valid_param(modify, [{foo, tags_clear}])).

test_invalid_params_check() ->
    Keys = read_api_keys(),
    ?assertEqual({error,{invalid_retrieve_params,[{foo,bar}]}},
                 erlpocket:retrieve(get_val(consumer_key, Keys),
                                    get_val(access_token, Keys), [{foo, bar}])),

    ?assertEqual({error,{invalid_add_params,[{foo,bar}]}},
                 erlpocket:add(get_val(consumer_key, Keys),
                               get_val(access_token, Keys), [{foo, bar}])),

    ?assertEqual({error,{invalid_modify_params,[{foo,bar}]}},
                 erlpocket:modify(get_val(consumer_key, Keys),
                                  get_val(access_token, Keys), [{foo, bar}])).

test_get_unreaded() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                        get_val(access_token, Keys),
                                        [{state, unread}]),
    ?assertEqual(ok, Result).

test_get_archive() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:retrieve(
                       get_val(consumer_key, Keys),
                       get_val(access_token, Keys),
                       [{state, archive}]
                      ),
    ?assertEqual(ok, Result).

test_get_favourite() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                        get_val(access_token, Keys),
                                        [{favorite, 1}]),
    ?assertEqual(ok, Result).

test_get_by_tag() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:retrieve(get_val(consumer_key, Keys),
                                        get_val(access_token, Keys),
                                        [{tag, erlang}]),
    ?assertEqual(ok, Result).

test_get_stats() ->
    Keys = read_api_keys(),
    Result = erlpocket:stats(get_val(consumer_key, Keys),
                             get_val(access_token, Keys)),
    ?assertMatch([{total_items, _},
                  {total_unread, _},
                  {total_archive, _},
                  {total_favorite, _},
                  {total_articles, _},
                  {total_videos, _}], Result).

test_add() ->
    Keys = read_api_keys(),
    {Result, _, _} = erlpocket:add(get_val(consumer_key, Keys),
                                   get_val(access_token, Keys),
                                   "http://www.erlang.org/", "test"),
    ?assertEqual(ok, Result).

test_favorite() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result1 = erlpocket:favorite(get_val(consumer_key, Keys),
                                 get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result1),

    Result2 = erlpocket:unfavorite(get_val(consumer_key, Keys),
                                   get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result2).

test_archive() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result1 = erlpocket:archive(get_val(consumer_key, Keys),
                                get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result1),
    Result2 = erlpocket:readd(get_val(consumer_key, Keys),
                              get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[_]},{<<"status">>,1}]}},
                 Result2).

test_tags_add() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:tags_add(get_val(consumer_key, Keys),
                                get_val(access_token, Keys), ItemId,
                                [<<"test">>, <<"test2">>]),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).

test_tags_remove() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:tags_remove(get_val(consumer_key, Keys),
                                   get_val(access_token, Keys),
                                   ItemId, [<<"test2">>]),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertEqual([], search_item(Keys, <<"test2">>)).

test_tags_replace() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:tags_replace(get_val(consumer_key, Keys),
                                    get_val(access_token, Keys),
                                    ItemId, [<<"test3">>]),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertNotEqual([], search_item(Keys, <<"test3">>)).

test_tag_rename() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:tag_rename(get_val(consumer_key, Keys),
                                  get_val(access_token, Keys),
                                  ItemId, <<"test3">>, <<"test4">>),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).

test_modify_tags_clear() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:tags_clear(get_val(consumer_key, Keys),
                                  get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result),
    ?assertEqual([], search_item(Keys, <<"test">>)).

test_modify_delete() ->
    Keys = read_api_keys(),
    [{ItemId, _} | _] = search_item(Keys),
    Result = erlpocket:delete(get_val(consumer_key, Keys),
                              get_val(access_token, Keys), ItemId),
    ?assertMatch({ok, _, {[{<<"action_results">>,[true]},{<<"status">>,1}]}},
                 Result).

%%%============================================================================
%%% Internal functionality
%%%============================================================================
read_api_keys() ->
    case file:consult("api.txt") of
        {ok, [Keys]} -> Keys;
        _ ->
            throw("Unable to read credentials from api.txt file!")
    end.

search_item(Keys) ->
    {ok, _, PL} = erlpocket:retrieve(
                      get_val(consumer_key, Keys),
                      get_val(access_token, Keys),
                      [{search, <<"Erlang Programming Language">>}]
                     ),
    parse_results(PL).

search_item(Keys, Tag) ->
    {ok, _, PL} = erlpocket:retrieve(
                      get_val(consumer_key, Keys),
                      get_val(access_token, Keys),
                      [{search, <<"Erlang Programming Language">>},
                       {tag, Tag}]),
    parse_results(PL).

parse_results({PL}) ->
    case get_val(<<"list">>, PL)  of
        []      -> [];
        {Items} -> lists:map(fun parse_result/1, Items)
    end.

parse_result([]) ->
    [];
parse_result({ItemId, {Item}}) ->
    {ItemId, Item}.

get_val(Key, PL) ->
    proplists:get_value(Key, PL).
