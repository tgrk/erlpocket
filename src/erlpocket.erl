%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket v3 API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket).

%% API
-export([
         request_token/2,
         get_authorize_url/2,
         authorize/2,

         stats/2,
         retrieve/3,
         add/3,
         add/4,
         add/5,
         modify/3,
         modify/4,

         delete/3,
         archive/3,
         readd/3,
         favorite/3,
         unfavorite/3,

         tags_add/4,
         tags_remove/4,
         tags_replace/4,
         tags_clear/3,
         tags_rename/5,

         is_valid_query/1,
         is_valid_param/2,

         start/0,
         stop/0
        ]).

-define(DEPS, [sasl, crypto, asn1, public_key, ssl, inets, jiffy, erlpocket]).
-define(BASE_URL, "https://getpocket.com/").

%% Types
-type result_stats() :: [{'total_archive' | 'total_articles' | 'total_favorite'
                          | 'total_items' | 'total_unread' | 'total_videos',
                          non_neg_integer()}].
-type params() :: list({atom(), any()}).

-export_type([result_stats/0, params/0]).


%%%============================================================================
%%% API
%%%============================================================================
-spec request_token(string(),string()) ->
                           {'ok', [{'code', string()}]} | {'error', any()}.
request_token(ConsumerKey, RedirectUri) ->
    call_api(request_token,
             jiffy:encode(
               {[{consumer_key, to_binary(ConsumerKey)},
                 {redirect_uri, to_binary(RedirectUri)}
                ]}
              ),
             params
            ).

-spec get_authorize_url([any()], string()) -> string().
get_authorize_url(Code, RedirectUri) ->
    get_url(authorize_url, Code, RedirectUri).

-spec authorize(string(),string()) ->
                       {'ok', list({atom(),any()})} | {'error', any()}.
authorize(ConsumerKey, Code) ->
    call_api(authorize,
             jiffy:encode(
               {[{consumer_key, to_binary(ConsumerKey)},
                 {code, to_binary(Code)}
                ]}
              ),
             params
            ).

-spec retrieve(string(),string(),[{_,_}]) ->
                      {'error',{'unable_to_retrieve',_}} | {'ok', _}.
retrieve(ConsumerKey, AccessToken, Query) ->
    case is_valid_query(Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(retrieve, Json, json) of
                {ok, JsonResp} ->
                    {ok, JsonResp};
                {error, Reason} ->
                    {error, {unable_to_retrieve, Reason}}
            end;
        false ->
            throw({invalid_retrieve_params, Query})
    end.

-spec stats(string(),string()) -> result_stats().
stats(ConsumerKey, AccessToken) ->
    Templates = [
                 {total_items,    [{state,       all}]},
                 {total_unread,   [{state,       unread}]},
                 {total_archive,  [{state,       archive}]},
                 {total_favorite, [{favourite,   1}]},
                 {total_articles, [{contentType, article}]},
                 {total_videos,   [{contentType, video}]}
                ],
    [get_stats(ConsumerKey, AccessToken, T) || T <- Templates].

-spec add(string(),string(),string(), string()) ->
                 {'error',{'unable_to_add',_}} | {'ok',_}.
add(ConsumerKey, AccessToken, Url, Tags) ->
    add(ConsumerKey, AccessToken,
        [{url, to_binary(Url)}, {tags, to_binary(Tags)}]).

-spec add(string(),string(),string(),string(),string()) ->
                 {'error',{'unable_to_add',_}} | {'ok',_}.
add(ConsumerKey, AccessToken, Url, Tags, TweetId) ->
    add(ConsumerKey, AccessToken, [{url,      to_binary(Url)},
                                   {tags,     to_binary(Tags)},
                                   {tweet_id, to_binary(TweetId)}
                                  ]).

-spec add(string(),string(), params()) ->
                 {'error',{'unable_to_add',_}} | {'ok',_}.
add(ConsumerKey, AccessToken, Query) ->
    case is_valid_param(add, Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(add, Json, json) of
                {ok, JsonResp} ->
                    {ok, JsonResp};
                {error, Reason} ->
                    {error, {unable_to_add, Reason}}
            end;
        false ->
            throw({invalid_add_params, Query})
    end.

-spec modify(string(),string(),params()) ->
                    {'error',{'unable_to_modify',_}} | {'ok',_}.
modify(ConsumerKey, AccessToken, Params) ->
    Json = get_json(ConsumerKey, AccessToken, Params),
    case call_api(modify, Json, json) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_modify, Reason}}
    end.

-spec modify(string(),string(),atom(),string()) ->
                    {'error',{'unable_to_modify',_}} | {'ok',_}.
modify(ConsumerKey, AccessToken, Action, ItemId) ->
    case modify(ConsumerKey, AccessToken,
           [{actions,
             [{[{action, Action}, {item_id, ItemId}]}]
            }]) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_modify, Reason}}
    end.

-spec delete(string(),string(),string()) ->
                    {'error',{'unable_to_delete',_}} | {'ok',_}.
delete(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, delete, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_delete, Reason}}
    end.

-spec archive(string(),string(),string()) ->
                     {'error',{'unable_to_archive',_}} | {'ok',_}.
archive(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, archive, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_delete, Reason}}
    end.

-spec readd(string(),string(),string()) ->
                   {'error',{'unable_to_readd',_}} | {'ok',_}.
readd(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, readd, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_readd, Reason}}
    end.

-spec favorite(string(),string(),string()) ->
                      {'error',{'unable_to_favorite',_}} | {'ok',_}.
favorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, favorite, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_favorite, Reason}}
    end.

-spec unfavorite(string(),string(),string()) ->
                        {'error',{'unable_to_unfavorite',_}} | {'ok',_}.
unfavorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, unfavorite, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_unfavorite, Reason}}
    end.

-spec tags_add(string(),string(),string(),list(binary())) ->
                      {'error',{'unable_to_tags_add',_}} | {'ok',_}.
tags_add(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_add, ItemId, Tags) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_tags_add, Reason}}
    end.

-spec tags_remove(string(),string(),string(),list(binary())) ->
                         {'error',{'unable_to_tags_remove',_}} | {'ok',_}.
tags_remove(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_remove, ItemId, Tags) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_tags_remove, Reason}}
    end.

-spec tags_replace(string(),string(),string(),list(binary())) ->
                          {'error',{'unable_to_tags_replace',_}} | {'ok',_}.
tags_replace(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_replace, ItemId, Tags) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_tags_replace, Reason}}
    end.

-spec tags_clear(string(),string(),string()) ->
                        {'error',{'unable_to_tags_clear',_}} | {'ok',_}.
tags_clear(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, tags_clear, ItemId) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, {unable_to_modify, Reason}} ->
            {error, {unable_to_tags_clear, Reason}}
    end.

-spec tags_rename(string(),string(),string(),string(),string()) ->
                         {'error',{'unable_to_tags_rename',_}} | {'ok',_}.
tags_rename(ConsumerKey, AccessToken, ItemId, OldTag, NewTag) ->
    case modify(ConsumerKey, AccessToken,
                [{actions,
                  [{[
                     {action,  tags_rename},
                     {item_id, ItemId},
                     {old_tag, eunsure_binary_list(OldTag)},
                     {new_tag, eunsure_binary_list(NewTag)}
                    ]}]
                 }]) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_tags_raname, Reason}}
    end.

-spec is_valid_query(params()) -> boolean().
is_valid_query(Filters) ->
    is_valid_param(retrieve, Filters).

-spec is_valid_param(atom(),params()) -> boolean().
is_valid_param(Type, Filters) ->
    not lists:member(
          false, [validate_filter(Type, F) || F <- Filters]
         ).

-spec start() -> 'ok'.
start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

-spec stop() -> 'ok'.
stop() ->
    [application:stop(A) || A <- ?DEPS],
    ok.


%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_items(ConsumerKey, AccessToken, Filter) ->
    {ok, {[{<<"status">>,   1},
           {<<"complete">>, 1},
           {<<"list">>,     {Items}},
           {<<"since">>,    _Since}
          ]}} = retrieve(ConsumerKey, AccessToken, Filter),
    Items.

get_stats(ConsumerKey, AccessToken, {Type, Params}) ->
    Items = get_items(ConsumerKey,
                      AccessToken,
                      lists:append(
                        Params,
                        [{detailType, simple}]
                       )
                     ),
    {Type, length(Items)}.

get_json(ConsumerKey, AccessToken, Params) ->
    jiffy:encode({
      lists:append([{consumer_key, to_binary(ConsumerKey)},
                    {access_token, to_binary(AccessToken)}
                   ],
                   Params
                  )
    }).

modify_tags(ConsumerKey, AccessToken, Action, ItemId, Tags) ->
    case modify(ConsumerKey, AccessToken,
                [{actions,
                  [{[
                     {action,  Action},
                     {item_id, ItemId},
                     {tags,    eunsure_binary_list(Tags)}
                    ]}]
                 }]) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {error, Reason} ->
            {error, {unable_to_tags_replace, Reason}}
    end.

%%TODO: header errors - http://getpocket.com/developer/docs/errors
call_api(UrlType, Json, Type) ->
   case http_request(get_url(UrlType), Json) of
        {200, Response} ->
           {ok, parse_response(Response, Type)};
        {400, _} ->
           {error, missing_required_parameters};
        {403, _} ->
            {error, invalid_consumer_key};
        {_Other, Reason} ->
            {error, Reason}
    end.

validate_filter(add, {url, _Value}) ->
    true;
validate_filter(add, {title, _Value}) ->
    true;
validate_filter(add, {tags, _Value}) ->
    true;
validate_filter(add, {tweet_id, _Value}) ->
    true;
validate_filter(add, {_Type, _Value}) ->
    false;
validate_filter(retrieve, {state, Value}) ->
    lists:member(Value, [unread, archive, all]);
validate_filter(retrieve, {favorite, Value}) ->
    lists:member(Value, [0, 1]);
validate_filter(retrieve, {tag, Value}) when is_atom(Value) ->
    true;
validate_filter(retrieve, {contentType, Value}) ->
    lists:member(Value, [article, image, video]);
validate_filter(retrieve, {sort, Value}) ->
    lists:member(Value, [newest, oldest, title, site]);
validate_filter(retrieve, {detailType, Value}) ->
    lists:member(Value, [complete, simple]);
validate_filter(retrieve, {search, Value}) when is_list(Value) ->
    false;
validate_filter(retrieve, {search, Value}) when is_binary(Value) ->
    true;
validate_filter(retrieve, {domain, Value}) when is_list(Value) ->
    false;
validate_filter(retrieve, {domain, Value}) when is_binary(Value) ->
    true;
validate_filter(retrieve, {since, _Value}) ->
    %TODO: implement timestamp validation
    true;
validate_filter(retrieve, {count, Value}) when is_integer(Value) ->
    true;
validate_filter(retrieve, {offset, Value}) when is_integer(Value)->
    true;
validate_filter(retrieve, {_Type, _Value}) ->
    false;
validate_filter(modify, {action, Value}) when is_atom(Value) ->
    true;
validate_filter(modify, {action, _Value}) ->
    false;
validate_filter(modify, {item_id, _Id}) ->
    true.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_binary(Response)).

http_request(Url, Json) ->
    case application:get_env(erlpocket, verbose, false) of
        {ok, true} ->
            io:format("erlpocket: call url=~p,json=~p~n", [Url, Json]);
        _ ->
            ignore
    end,
    {ok, {{_, Status, _}, _, Response}} =
        httpc:request(
          post,
          {Url, ["application/json"], "application/json", Json},
          [{timeout, infinity}],
          []
         ),

    {Status, Response}.

parse_params(Input) ->
    lists:map(fun parse_param/1, string:tokens(Input, "&")).

parse_param(Input) ->
    [Key, Value] = string:tokens(Input, "="),
    {list_to_atom(Key), Value}.

get_url(request_token) ->
    ?BASE_URL ++ "v3/oauth/request";
get_url(authorize) ->
    ?BASE_URL ++ "v3/oauth/authorize";
get_url(retrieve) ->
    ?BASE_URL ++ "v3/get";
get_url(add) ->
    ?BASE_URL ++ "v3/add";
get_url(modify) ->
    ?BASE_URL ++ "v3/send".

get_url(authorize_url, Code, RedirectUri) ->
    ?BASE_URL ++ "auth/authorize?request_token=" ++ Code ++ ""
        "&redirect_uri=" ++ RedirectUri.

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) ->
    Value.

eunsure_binary_list(List) ->
    [to_binary(I) || I <- List].
