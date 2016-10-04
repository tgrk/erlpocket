%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 7 Apr 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket_tests).

-export([erlpocket_test_/0]).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
erlpocket_test_() ->
    {setup,
     fun() ->
             application:ensure_all_started(erlpocket),
             true = maybe_mock_api(has_api_key())
     end,
     fun(_) ->
             maybe_unmock_api(has_api_key())
     end,
     [
       {timeout, 100, {"Custom oAuth test",         fun test_oauth/0}}
     , {timeout, 100, {"Unauthorized call",         fun test_unauth_get/0}}
     , {timeout, 600, {"Retrieve all items",        fun test_get_all/0}}
     , {timeout, 100, {"Validate params",           fun test_validate_params/0}}
     , {timeout, 100, {"Invalid params check",      fun test_invalid_params_check/0}}
     , {timeout, 100, {"Test maps support",         fun test_maps_support/0}}
     , {timeout, 100, {"Retrieve unreaded items",   fun test_get_unreaded/0}}
     , {timeout, 100, {"Retrieve archived items",   fun test_get_archive/0}}
     , {timeout, 100, {"Retrieve favourite items",  fun test_get_favourite/0}}
     , {timeout, 100, {"Retrieve items by tag",     fun test_get_by_tag/0}}
     , {timeout, 300, {"Retrieve items stats",      fun test_get_stats/0}}
     , {timeout, 100, {"Add a new item",            fun test_add/0}}
     , {timeout, 100, {"Mark/unmark item favorite", fun test_favorite/0}}
     , {timeout, 100, {"Archive/read item",         fun test_archive/0}}
     , {timeout, 100, {"Add tags to an item",       fun test_tags_add/0}}
     , {timeout, 100, {"Remove item's tag",         fun test_tags_remove/0}}
     , {timeout, 100, {"Replace item's tag",        fun test_tags_replace/0}}
     , {timeout, 100, {"Rename item's tag",         fun test_tag_rename/0}}
     , {timeout, 100, {"Remove item's tags",        fun test_modify_tags_clear/0}}
     , {timeout, 100, {"Delete an existing item",   fun test_modify_delete/0}}
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

    ok = maybe_mock_api_call(authorize, []),

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

    ok = maybe_mock_api_call(unauthorized, []),

    {Result, _} = erlpocket:retrieve(get_val(consumer_key, Keys), "foo", []),
    ?assertEqual(error, Result).

test_get_all() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, []),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, []),

    ?assertEqual(ok, Result).

test_validate_params() ->
    ?assertEqual(true, erlpocket:is_valid_param(add, [{url, "http://foobar"}])),
    ?assertEqual(true, erlpocket:is_valid_param(modify, [{action, tags_clear}])),
    ?assertEqual(false, erlpocket:is_valid_query([{url, "http://foobar"}])),
    ?assertEqual(false, erlpocket:is_valid_param(modify, [{foo, tags_clear}])).

test_invalid_params_check() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(badformat, []),

    ?assertEqual({error,{invalid_retrieve_params,[{foo,bar}]}},
                 erlpocket:retrieve(CKey, AToken, [{foo, bar}])),

    ?assertEqual({error,{invalid_add_params,[{foo,bar}]}},
                 erlpocket:add(CKey, AToken, [{foo, bar}])),

    ?assertEqual({error,{invalid_modify_params,[{foo,bar}]}},
                 erlpocket:modify(CKey, AToken, [{foo, bar}])).

test_maps_support() ->
    {CKey, AToken} = credentials(),

    ok = application:set_env(erlpocket, return_maps, true),

    ?assertEqual({ok, true}, application:get_env(erlpocket, return_maps)),

    ok = maybe_mock_api_call(retrieve, []),

    {ok, Headers, Results} = erlpocket:retrieve(CKey, AToken, [{state, archive}]),

    ?assert(erlang:is_map(Headers)),
    ?assert(erlang:is_map(Results)),

    application:set_env(erlpocket, return_maps, false),
    ok.

test_get_unreaded() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, {state, unread}),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, [{state, unread}]),

    ?assertEqual(ok, Result).

test_get_archive() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, {state, archive}),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, [{state, archive}]),

    ?assertEqual(ok, Result).

test_get_favourite() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, {favorite, 1}),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, [{favorite, 1}]),

    ?assertEqual(ok, Result).

test_get_by_tag() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, {tag, erlang}),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, [{tag, erlang}]),
    ?assertEqual(ok, Result).

test_get_stats() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(stats, []),

    Result = erlpocket:stats(CKey, AToken),

    ?assertMatch([{total_items, _},
                  {total_unread, _},
                  {total_archive, _},
                  {total_favorite, _},
                  {total_articles, _},
                  {total_videos, _}], Result).

test_add() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(add, []),

    {Result, _, _} = erlpocket:add(CKey, AToken, "http://www.erlang.org/", "test"),

    ?assertEqual(ok, Result).

test_favorite() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result1 = erlpocket:favorite(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result1),

    Result2 = erlpocket:unfavorite(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result2),
    ok.

test_archive() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result1 = erlpocket:archive(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result1),

    Result2 = erlpocket:readd(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result2),
    ok.

test_tags_add() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:tags_add(CKey, AToken, ItemId, [<<"test">>, <<"test2">>]),
    assert_action_response(true, 1, Result),
    ok.

test_tags_remove() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:tags_remove(CKey, AToken, ItemId, [<<"test2">>]),
    assert_action_response(true, 1, Result),


    ?assertEqual([], search_item(Creds, <<"test2">>)),
    ok.

test_tags_replace() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:tags_replace(CKey, AToken, ItemId, [<<"test3">>]),
    assert_action_response(true, 1, Result),
    ok.

test_tag_rename() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:tag_rename(CKey, AToken, ItemId, <<"test3">>, <<"test4">>),
    assert_action_response(true, 1, Result),
    %%?assertNotEqual([], search_item(Keys, <<"test4">>)),
    ok.

test_modify_tags_clear() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:tags_clear(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result),

    ?assertEqual([], search_item(Creds, <<"test">>)),
    ok.

test_modify_delete() ->
    {CKey, AToken} = Creds = credentials(),

    [{ItemId, _} | _] = search_item(Creds),

    ok = maybe_mock_api_call(modify, []),

    Result = erlpocket:delete(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result),
    ok.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
maybe_mock_api(true) ->
    true;
maybe_mock_api(false) ->
    meck:new(httpc, [passthrough]),
    meck:validate(httpc).

maybe_unmock_api(true) ->
    ok;
maybe_unmock_api(false) ->
    meck:unload(httpc).

maybe_mock_api_call(Type, Params) ->
    maybe_mock_api_call(Type, Params, []).

maybe_mock_api_call(Type, Params, Args) ->
    case has_api_key() of
        false ->
            IgnoreTypes = [stats, badformat, unauthorized],
            meck:expect(
              httpc, request,
              fun(Verb, {Url, [], _, Json}, _Opts, []) ->
                      ?assertEqual(post, Verb),

                      %% maybe match request url
                      case lists:member(Type, IgnoreTypes) of
                          true  -> ignore;
                          false -> ?assertEqual(erlpocket:get_url(Type), Url)
                      end,

                      %% maybe match request params and payload
                      case Params of
                          {Key, Value} ->
                              Map = jiffy:decode(Json, [return_maps]),
                              ?assertEqual(erlpocket:to_binary(Value),
                                           maps:get(erlpocket:to_binary(Key), Map));
                          [] ->
                              ignore
                      end,
                      get_response(Type, Args)
              end);
        true ->
            ok
    end.

get_response(authorize, _Args) ->
    Headers = [{"x-error","User rejected code."},
               {"x-error-code","158"},
               {"x-limit-key-limit",[]},
               {"x-limit-key-remaining",[]},
               {"x-limit-key-reset",[]},
               {"x-limit-user-limit",[]},
               {"x-limit-user-remaining",[]},
               {"x-limit-user-reset",[]},
               {"x-source","Pocket"}],
    build_raw_response(403, Headers, []);
get_response(unauthorized, _Args) ->
    build_raw_response(403, [], []);
get_response(badformat, _Args) ->
    build_raw_response(400, [], []);
get_response(retrieve, Args) ->
    Payload = #{<<"status">>   => 2,
                <<"complete">> => 1,
                <<"list">>     => Args,
                <<"since">>    => 1475443593
               },
    build_raw_response(200, [], Payload);
get_response(modify, _Args) ->
    Payload = #{<<"action_results">> => [true],
                <<"status">>         => 1
               },
    build_raw_response(200, [], Payload);
get_response(_Type, _Args) ->
    build_raw_response(200, [], []).

build_raw_response(Code, Headers, Response) ->
    {ok, {{undefined, Code, undefined}, Headers, jiffy:encode(Response)}}.

read_api_keys() ->
    case file:consult("api.txt") of
        {ok, [Keys]} -> Keys;
        _ -> []
    end.

has_api_key() ->
    %filelib:is_regular("api.txt").
    false.

assert_action_response(ExpectedResult, ExpectedStatus, {Code, _Headers, {Result}}) ->
    ?assertEqual(ok, Code),
    ?assertEqual([ExpectedResult], get_val(<<"action_results">>, Result)),
    ?assertEqual(ExpectedStatus,   get_val(<<"status">>, Result)),
    ok.

credentials() ->
    Keys = read_api_keys(),
    {get_val(consumer_key, Keys), get_val(access_token, Keys)}.

search_item({CKey, AToken}) ->
    ok = maybe_mock_api_call(retrieve, [], #{<<"fooId">> => #{}}),

    {ok, _, PL} = erlpocket:retrieve(CKey, AToken, [{search, <<"Erlang Programming Language">>}]),

    parse_results(PL).

search_item({CKey, AToken}, Tag) ->
    ok = maybe_mock_api_call(retrieve, [], []),

    {ok, _, PL} = erlpocket:retrieve(CKey, AToken, [{search, <<"Erlang Programming Language">>},
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
