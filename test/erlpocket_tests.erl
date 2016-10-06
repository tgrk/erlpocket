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

    ok = maybe_mock_api_call(request_token, #{}, <<"code=foobar">>),

    Url = <<"http://https://github.com/tgrk/erlpocket/">>,
    {ok, _Headers, Map} = erlpocket:request_token(
                            maps:get(consumer_key, Keys), Url),

    Code = maps:get(code, Map),
    AuthUrl = erlpocket:get_authorize_url(Code, Url),
    [_, Args] = binary:split(AuthUrl, <<"?">>),
    [Arg1, Arg2] = binary:split(Args, <<"&">>),
    ?assertEqual(<<"request_token=", (Code)/binary>>, Arg1),
    ?assertEqual(<<"redirect_uri=", (Url)/binary>>, Arg2),

    ok = maybe_mock_api_call(authorize, #{}),

    %% since we didn't click on authorization url, final oAuth step should fail
    ?assertEqual(
       {error,{invalid_consumer_key,
               #{<<"x-error">>      => <<"User rejected code.">>,
                 <<"x-error-code">> => <<"158">>,
                 <<"x-source">>     => <<"Pocket">>}}},
       erlpocket:authorize(maps:get(consumer_key, Keys), Code)).

test_unauth_get() ->
    Keys = read_api_keys(),

    ok = maybe_mock_api_call(unauthorized, #{}),

    {Result, _} = erlpocket:retrieve(maps:get(consumer_key, Keys), <<"foo">>, #{}),
    ?assertEqual(error, Result).

test_get_all() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(retrieve, #{}),

    {Result, _, _} = erlpocket:retrieve(CKey, AToken, #{}),

    ?assertEqual(ok, Result).

test_validate_params() ->
    ?assertEqual(true,  erlpocket:is_valid_param(add,    #{url => <<"http://foobar">>})),
    ?assertEqual(true,  erlpocket:is_valid_param(modify, #{action => tags_clear})),
    ?assertEqual(false, erlpocket:is_valid_query(#{url => <<"http://foobar">>})),
    ?assertEqual(false, erlpocket:is_valid_param(modify, #{foo => tags_clear})).

test_invalid_params_check() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(badformat, #{}),

    ?assertEqual({error, invalid_params},
                 erlpocket:retrieve(CKey, AToken, #{foo => bar})),

    ?assertEqual({error, invalid_params},
                  erlpocket:add(CKey, AToken, #{foo => bar})),

    ?assertEqual({error, invalid_params},
                 erlpocket:modify(CKey, AToken, #{foo => bar})).

test_maps_support() ->
    {CKey, AToken} = credentials(),

    ok = application:set_env(erlpocket, return_maps, true),

    ?assertEqual({ok, true}, application:get_env(erlpocket, return_maps)),

    ok = maybe_mock_api_call(retrieve, #{}),

    {ok, Headers, Results} = erlpocket:retrieve(CKey, AToken, #{state => archive}),

    ?assert(erlang:is_map(Headers)),
    ?assert(erlang:is_map(Results)),

    application:set_env(erlpocket, return_maps, false),
    ok.

test_get_unreaded() ->
    {CKey, AToken} = credentials(),

    Param = #{state => unread},
    ok = maybe_mock_api_call(retrieve, Param),
    {Result, _, _} = erlpocket:retrieve(CKey, AToken, Param),

    ?assertEqual(ok, Result).

test_get_archive() ->
    {CKey, AToken} = credentials(),

    Param = #{state => archive},
    ok = maybe_mock_api_call(retrieve, Param),
    {Result, _, _} = erlpocket:retrieve(CKey, AToken, Param),

    ?assertEqual(ok, Result).

test_get_favourite() ->
    {CKey, AToken} = credentials(),

    Param = #{favorite => 1},
    ok = maybe_mock_api_call(retrieve, Param),
    {Result, _, _} = erlpocket:retrieve(CKey, AToken, Param),

    ?assertEqual(ok, Result).

test_get_by_tag() ->
    {CKey, AToken} = credentials(),

    Param = #{tag => <<"erlang">>},
    ok = maybe_mock_api_call(retrieve, Param),
    {Result, _, _} = erlpocket:retrieve(CKey, AToken, Param),

    ?assertEqual(ok, Result).

test_get_stats() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(stats, #{}),

    {ok, Result} = erlpocket:stats(CKey, AToken),
    ExpectedKeys = [total_items, total_unread, total_archive, total_favorite,
                    total_articles, total_videos],

    ?assertEqual(lists:sort(ExpectedKeys), lists:sort(maps:keys(Result))).

test_add() ->
    {CKey, AToken} = credentials(),

    ok = maybe_mock_api_call(add, #{}),
    {Result, _, _} = erlpocket:add(CKey, AToken, "http://www.erlang.org/", "test"),

    ?assertEqual(ok, Result).

test_favorite() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result1 = erlpocket:favorite(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result1),

    Result2 = erlpocket:unfavorite(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result2),
    ok.

test_archive() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result1 = erlpocket:archive(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result1),

    Result2 = erlpocket:readd(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result2),
    ok.

test_tags_add() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result = erlpocket:tags_add(CKey, AToken, ItemId, [<<"test">>, <<"test2">>]),
    assert_action_response(true, 1, Result),
    ok.

test_tags_remove() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result = erlpocket:tags_remove(CKey, AToken, ItemId, [<<"test2">>]),
    assert_action_response(true, 1, Result),

    ok.

test_tags_replace() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result = erlpocket:tags_replace(CKey, AToken, ItemId, [<<"test3">>]),
    assert_action_response(true, 1, Result),
    ok.

test_tag_rename() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result = erlpocket:tag_rename(CKey, AToken, ItemId, <<"test3">>, <<"test4">>),
    assert_action_response(true, 1, Result),
    %%?assertNotEqual([], search_item(Keys, <<"test4">>)),

    ok.

test_modify_tags_clear() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

    Result = erlpocket:tags_clear(CKey, AToken, ItemId),
    assert_action_response(true, 1, Result),
    ok.

test_modify_delete() ->
    {CKey, AToken} = Creds = credentials(),

    ItemId = search_item(Creds),

    ok = maybe_mock_api_call(modify, #{}),

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
    maybe_mock_api_call(Type, Params, #{}).

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
                          false -> ?assertEqual(erlpocket:get_url(Type), list_to_binary(Url))
                      end,

                      %% maybe match request params and payload
                      case maps:size(Params) > 0 of
                          true ->
                              %%TODO: this expects only one param and is not ideal
                              [{Key, Value}] = maps:to_list(Params),
                              Map = jiffy:decode(Json, [return_maps]),
                              ?assertEqual(erlpocket:to_binary(Value),
                                           maps:get(erlpocket:to_binary(Key), Map));
                          false ->
                              ignore
                      end,
                      get_response(Type, Args)
              end);
        true ->
            ok
    end.

get_response(authorize, _Args) ->
    Headers = #{"x-error"      => "User rejected code.",
                "x-error-code" => "158",
                "x-source"     => "Pocket"},
    build_raw_response(403, maps:to_list(Headers), #{});
get_response(unauthorized, _Args) ->
    build_raw_response(403, [], #{});
get_response(badformat, _Args) ->
    build_raw_response(400, [], #{});
get_response(request_token, Args) ->
    build_raw_response(200, [], Args);
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
    build_raw_response(200, [], #{}).

build_raw_response(Code, Headers, Params) when is_binary(Params) ->
    {ok, {{undefined, Code, undefined}, Headers, binary_to_list(Params)}};
build_raw_response(Code, Headers, Response) ->
    {ok, {{undefined, Code, undefined}, Headers, jiffy:encode(Response)}}.

read_api_keys() ->
    case file:consult("api.txt") of
        {ok, [Keys]} -> Keys;
        _ -> []
    end.

has_api_key() ->
    filelib:is_regular("api.txt").

assert_action_response(_ExpectedResult, ExpectedStatus, {Code, _Headers, Result}) ->
    ?assertEqual(ok, Code),
    ?assertMatch([_ExpectedResult], maps:get(<<"action_results">>, Result)),
    ?assertEqual(ExpectedStatus,   maps:get(<<"status">>, Result)),
    ok.

credentials() ->
    Map = read_api_keys(),
    {maps:get(consumer_key, Map), maps:get(access_token, Map)}.

search_item({CKey, AToken}) ->
    ok = maybe_mock_api_call(retrieve, #{}, #{<<"fooId">> => #{}}),

    {ok, _, Map} = erlpocket:retrieve(
                    CKey, AToken, #{search => <<"Erlang Programming Language">>}),

    hd(maps:keys(parse_results(Map))).

parse_results(Map) ->
    maps:get(<<"list">>, Map).
