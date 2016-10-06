%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket v3 API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket).

%% API
-export([ request_token/2
        , get_authorize_url/2
        , authorize/2

        , stats/2
        , retrieve/3
        , add/3
        , add/4
        , add/5
        , modify/3
        , modify/4

        , delete/3
        , archive/3
        , readd/3
        , favorite/3
        , unfavorite/3

        , tags_add/4
        , tags_remove/4
        , tags_replace/4
        , tags_clear/3
        , tag_rename/5

        , build_batch_query/1
        , is_valid_query/1
        , is_valid_param/2
        ]).

%% for testing only
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([get_url/1, to_binary/1]).
-endif.

-define(BASE_URL, <<"https://getpocket.com/">>).

%% Types
-type oauth_key()    :: string() | binary().
-type oauth_token()  :: string() | binary().
-type headers()      :: map().
-type result_ok()    :: {ok, headers(), map()} | {ok, map()}.
-type result_error() :: {error, {atom(), headers()}} | {error, atom()}.
-type action()       :: atom().
-type query()        :: map().
-type url()          :: string() | binary().
-type tag()          :: string() | binary().
-type tags()         :: list(tag()).
-type item_id()      :: string() | binary().

-export_type([oauth_key/0, oauth_token/0, result_ok/0, result_error/0]).
-export_type([action/0, query/0, item_id/0, url/0, tag/0, tags/0]).

%%%============================================================================
%%% API
%%%============================================================================
-spec request_token(oauth_key(), url()) -> {ok, map()} | result_error().
request_token(ConsumerKey, RedirectUri) ->
    Payload = jiffy:encode(#{consumer_key => to_binary(ConsumerKey),
                             redirect_uri => to_binary(RedirectUri)}),
    call_api(request_token, Payload, params).

-spec get_authorize_url(binary(), url()) -> url().
get_authorize_url(Code, RedirectUri) ->
    get_url(authorize_url, Code, RedirectUri).

-spec authorize(oauth_key(), string()) -> {ok, map()} | result_error().
authorize(ConsumerKey, Code) ->
    Payload = jiffy:encode(#{consumer_key => to_binary(ConsumerKey),
                             code         => to_binary(Code)}),
    call_api(authorize, Payload, params).

-spec retrieve(oauth_key(), oauth_token(), query()) ->
                      result_ok() | result_error().
retrieve(ConsumerKey, AccessToken, Query) ->
    case is_valid_query(Query) of
        true ->
            Payload = create_payload(ConsumerKey, AccessToken, Query),
            call_api(retrieve, Payload, json);
        false ->
            {error, invalid_params}
    end.

-spec stats(oauth_key(), oauth_token()) -> result_ok().
stats(ConsumerKey, AccessToken) ->
    Templates = #{total_items    => #{state       => all},
                  total_unread   => #{state       => unread},
                  total_archive  => #{state       => archive},
                  total_favorite => #{favorite    => 1},
                  total_articles => #{contentType => article},
                  total_videos   => #{contentType => video}
                 },
    Stats = maps:map(fun (_Name, T) ->
                             get_stats(ConsumerKey, AccessToken, T)
                     end, Templates),
    {ok, Stats}.

%%TODO: check types
-spec add(oauth_key(), oauth_token(), url(), tag()) ->
                 result_ok() | result_error().
add(ConsumerKey, AccessToken, Url, Tags) ->
    add(ConsumerKey, AccessToken, #{url  => to_binary(Url),
                                    tags => to_binary(Tags)}).

-spec add(oauth_key(), oauth_token(), url(), string(), string()) ->
                 result_ok() | result_error().
add(ConsumerKey, AccessToken, Url, Tags, TweetId) ->
    add(ConsumerKey, AccessToken, #{url     => to_binary(Url),
                                   tags     => to_binary(Tags),
                                   tweet_id => to_binary(TweetId)}).

-spec add(oauth_key(), oauth_token(), query()) ->
                 result_ok() | result_error().
add(ConsumerKey, AccessToken, Query) ->
    case is_valid_param(add, Query) of
        true ->
            Payload = create_payload(ConsumerKey, AccessToken, Query),
            case call_api(add, Payload, json) of
                {ok, Headers, Response} ->
                    {ok, Headers, Response};
                {error, Headers} ->
                    {error, {unable_to_add, Headers}}
            end;
        false ->
            {error, invalid_params}
    end.

-spec modify(oauth_key(), oauth_token(), query()) ->
                    result_ok() | result_error().
modify(ConsumerKey, AccessToken, Query) ->
    case is_valid_param(modify, Query) of
        true ->
            Payload = create_payload(ConsumerKey, AccessToken, Query),
            case call_api(modify, Payload, json) of
                {ok, Headers, Response} ->
                    {ok, Headers, Response};
                {error, Headers} ->
                    {error, {unable_to_modify, Headers}}
            end;
        false ->
            {error, invalid_params}
    end.

-spec modify(oauth_key(), oauth_token(), action(), item_id()) ->
                    result_ok() | result_error().
modify(ConsumerKey, AccessToken, Action, ItemId) ->
    Query = build_batch_query([{Action, ItemId, #{}}]),
    modify(ConsumerKey, AccessToken, Query).

-spec delete(oauth_key(), oauth_token(), item_id()) ->
                    result_ok() | result_error().
delete(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, delete, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_delete, Headers}}
    end.

-spec archive(oauth_key(), oauth_token(), item_id()) ->
                     result_ok() | result_error().
archive(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, archive, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_archive, Headers}}
    end.

-spec readd(oauth_key(), oauth_token(), item_id()) ->
                   result_ok() | result_error().
readd(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, readd, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_readd, Headers}}
    end.

-spec favorite(oauth_key(), oauth_token(), item_id()) ->
                      result_ok() | result_error().
favorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, favorite, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_favorite, Headers}}
    end.

-spec unfavorite(oauth_key(), oauth_token(), item_id()) ->
                        result_ok() | result_error().
unfavorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, unfavorite, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_unfavorite, Headers}}
    end.

-spec tags_add(oauth_key(), oauth_token(), item_id(), tags()) ->
                      result_ok() | result_error().
tags_add(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_add, ItemId, Tags) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_add_tags, Headers}}
    end.

-spec tags_remove(oauth_key(), oauth_token(), item_id(), tags()) ->
                         result_ok() | result_error().
tags_remove(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_remove, ItemId, Tags) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_remove_tags, Headers}}
    end.

-spec tags_replace(oauth_key(), oauth_token(), item_id(), tags()) ->
                          result_ok() | result_error().
tags_replace(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_replace, ItemId, Tags) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_replace_tags, Headers}}
    end.

-spec tags_clear(oauth_key(), oauth_token(), item_id()) ->
                        result_ok() | result_error().
tags_clear(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, tags_clear, ItemId) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_clear_tags, Headers}}
    end.

-spec tag_rename(oauth_key(), oauth_token(), item_id(), tag(), tag()) ->
                        result_ok() | result_error().
tag_rename(ConsumerKey, AccessToken, ItemId, OldTag, NewTag) ->
    Args = #{old_tag => to_binary(OldTag),
             new_tag => to_binary(NewTag)},
    Query = build_batch_query([{tag_rename, ItemId, Args}]),
    case modify(ConsumerKey, AccessToken, Query) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_raname_tags, Headers}};
        {error, _Reason} = Error ->
            Error
    end.

-spec build_batch_query(list(tuple())) -> map().
build_batch_query([Action]) ->
    #{actions => [build_query_action(Action)]};
build_batch_query(Actions) ->
    #{actions => [build_query_action(A) || A <- Actions]}.

-spec is_valid_query(query()) -> boolean().
is_valid_query(Query) ->
    is_valid_param(retrieve, Query).

-spec is_valid_param(atom(), query()) -> boolean().
is_valid_param(Type, Query) ->
    Results = [validate_filter(Type, K, V) || {K, V} <- maps:to_list(Query)],
    not lists:member(false, Results).

%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_items(ConsumerKey, AccessToken, FilterQuery) ->
    case retrieve(ConsumerKey, AccessToken, FilterQuery) of
        {ok, _Headers, Response} ->
            maps:get(<<"list">>, Response, #{});
        Other ->
            maybe_verbose("unable to retrieve items: ~p~n", [Other]),
            #{}
    end.

get_stats(ConsumerKey, AccessToken, Params) ->
    FilterQuery = maps:put(detailType, simple, Params),
    maps:size(get_items(ConsumerKey, AccessToken, FilterQuery)).

create_payload(ConsumerKey, AccessToken, Params) ->
    Defaults = #{consumer_key => to_binary(ConsumerKey),
                 access_token => to_binary(AccessToken)
                },
    jiffy:encode(maps:merge(Defaults, Params)).

modify_tags(ConsumerKey, AccessToken, Action, ItemId, Tags) ->
    Query = build_batch_query(
              [{Action, ItemId, #{tags => ensure_binary_list(Tags)}}]),
    case modify(ConsumerKey, AccessToken, Query) of
        {ok, Headers, Response} ->
            {ok, Headers, Response};
        {error, _Reason} = Error ->
            Error
    end.

build_query_action({Action, ItemId, Args}) ->
    Defaults = #{action => Action, item_id => ItemId},
    maps:merge(Defaults, Args).

call_api(UrlType, Payload, Type) ->
   case http_request(get_url(UrlType), Payload) of
       {200, Headers, Response} ->
           {ok, Headers, parse_response(Response, Type)};
       {400, Headers, _} ->
           {error, {missing_required_parameters, Headers}};
       {403, Headers, _} ->
           {error, {invalid_consumer_key, Headers}};
       {_Other, Headers, Reason} ->
           {error, {Reason, Headers}}
   end.

http_request(Url, Payload) ->
    maybe_verbose("call url=~p,json=~p~n", [Url, Payload]),
    {ok, {{_, Status, _}, Headers, Response}} =
        httpc:request(post,
                      {binary_to_list(Url), [], "application/json", Payload},
                      [{timeout, infinity}], []),
    {Status, filter_headers(Headers), Response}.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_binary(Response), [return_maps]).

parse_params(Input) ->
    lists:foldl(fun (Arg, Acc) -> parse_param(Arg, Acc) end, #{}, string:tokens(Input, "&")).

parse_param(Input, Acc) ->
    [Key, Value] = string:tokens(Input, "="),
    maps:put(list_to_atom(Key), to_binary(Value), Acc).

filter_headers(L) ->
    Keys = ["x-error", "x-error-code", "x-limit-key-limit",
            "x-limit-key-remaining", "x-limit-key-reset",
            "x-limit-user-limit", "x-limit-user-remaining",
            "x-limit-user-reset", "x-source"],
    maps:from_list(
      lists:foldl(fun ({K, V}, Acc) ->
                          case lists:member(K, Keys) of
                              true  -> [{to_binary(K), to_binary(V)} | Acc];
                              false -> Acc
                          end
                  end, [], L)).

validate_filter(add, url, _Value) ->
    true;
validate_filter(add, title, _Value) ->
    true;
validate_filter(add, tags, _Value) ->
    true;
validate_filter(add, tweet_id, _Value) ->
    true;
validate_filter(retrieve, state, Value) ->
    lists:member(Value, [unread, archive, all]);
validate_filter(retrieve, favorite, Value) ->
    lists:member(Value, [0, 1]);
validate_filter(retrieve, tag, Value) when is_atom(Value) ->
    true;
validate_filter(retrieve, tag, Value) when is_binary(Value) ->
    true;
validate_filter(retrieve, contentType, Value) ->
    lists:member(Value, [article, image, video]);
validate_filter(retrieve, sort, Value) ->
    lists:member(Value, [newest, oldest, title, site]);
validate_filter(retrieve, detailType, Value) ->
    lists:member(Value, [complete, simple]);
validate_filter(retrieve, search, Value) when is_binary(Value) ->
    true;
validate_filter(retrieve, domain, Value) when is_binary(Value) ->
    true;
validate_filter(retrieve, since, Value)
  when is_integer(Value) andalso Value > 0 ->
    true;
validate_filter(retrieve, count, Value) when is_integer(Value) ->
    true;
validate_filter(retrieve, offset, Value) when is_integer(Value)->
    true;
validate_filter(modify, actions, Value) when is_list(Value) ->
    %TODO: implement validation of nested params
    true;
validate_filter(modify, action, Value) when is_atom(Value) ->
    true;
validate_filter(modify, item_id, _Id) ->
    true;
validate_filter(_, _, _) ->
    false.

maybe_verbose(Text, Args) ->
    case application:get_env(erlpocket, verbose, false) of
        true ->
            io:format("erlpocket: " ++ Text, Args);
        false ->
            ignore
    end.

get_url(request_token) ->
    <<(?BASE_URL)/binary, "v3/oauth/request">>;
get_url(authorize) ->
    <<(?BASE_URL)/binary, "v3/oauth/authorize">>;
get_url(retrieve) ->
    <<(?BASE_URL)/binary, "v3/get">>;
get_url(add) ->
    <<(?BASE_URL)/binary, "v3/add">>;
get_url(modify) ->
    <<(?BASE_URL)/binary, "v3/send">>.

get_url(authorize_url, Code, RedirectUri) ->
    <<(?BASE_URL)/binary, "auth/authorize?request_token=",
      (Code)/binary, "&redirect_uri=", (RedirectUri)/binary>>.

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
to_binary(Value) ->
    Value.

ensure_binary_list(List) ->
    [to_binary(I) || I <- List].
