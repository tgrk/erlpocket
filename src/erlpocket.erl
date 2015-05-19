%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket v3 API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket).

%% API
-export([request_token/2,
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
         tag_rename/5,

         is_valid_query/1,
         is_valid_param/2,

         start/0,
         stop/0
        ]).

-define(DEPS, [crypto, asn1, public_key, ssl, inets, jiffy, erlpocket]).
-define(BASE_URL, "https://getpocket.com/").

%% Types
-type headers()      :: list({any(), any()}).
-type result_ok()    :: {ok, headers(), any()}.
-type result_error() :: {error, {atom(), headers()}}.
-type result_stats() :: [{total_archive | total_articles | total_favorite
                          | total_items | total_unread | total_videos,
                          non_neg_integer()}].
-type params()       :: list({atom(), any()}).

-export_type([result_ok/0, result_error/0, headers/0, result_stats/0,
              params/0]).

%%%============================================================================
%%% API
%%%============================================================================
-spec request_token(string(),string()) ->
                           {ok, [{code, string()}]} | {error, any()}.
request_token(ConsumerKey, RedirectUri) ->
    call_api(request_token,
             jiffy:encode({[{consumer_key, to_binary(ConsumerKey)},
                            {redirect_uri, to_binary(RedirectUri)}]}), params).

-spec get_authorize_url([any()], string()) -> string().
get_authorize_url(Code, RedirectUri) ->
    get_url(authorize_url, Code, RedirectUri).

-spec authorize(string(),string()) ->
                       {ok, list({atom(),any()})} | {error, any()}.
authorize(ConsumerKey, Code) ->
    call_api(authorize,
             jiffy:encode({[{consumer_key, to_binary(ConsumerKey)},
                            {code, to_binary(Code)}]}), params).

-spec retrieve(string(),string(),[{_,_}]) -> result_ok() | result_error().
retrieve(ConsumerKey, AccessToken, Query) ->
    case is_valid_query(Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(retrieve, Json, json) of
                {ok, Headers, JsonResp} ->
                    {ok, Headers, JsonResp};
                {error, Headers} ->
                    {error, {unable_to_retrieve, Headers}}
            end;
        false ->
            {error, {invalid_retrieve_params, Query}}
    end.

-spec stats(string(),string()) -> result_stats().
stats(ConsumerKey, AccessToken) ->
    Templates = [{total_items,    [{state,       all}]},
                 {total_unread,   [{state,       unread}]},
                 {total_archive,  [{state,       archive}]},
                 {total_favorite, [{favorite,    1}]},
                 {total_articles, [{contentType, article}]},
                 {total_videos,   [{contentType, video}]}],
    [get_stats(ConsumerKey, AccessToken, T) || T <- Templates].

-spec add(string(),string(),string(),string()) -> result_ok() | result_error().
add(ConsumerKey, AccessToken, Url, Tags) ->
    add(ConsumerKey, AccessToken,
        [{url, to_binary(Url)}, {tags, to_binary(Tags)}]).

-spec add(string(),string(),string(),string(),string()) ->
                 result_ok() | result_error().
add(ConsumerKey, AccessToken, Url, Tags, TweetId) ->
    add(ConsumerKey, AccessToken, [{url,      to_binary(Url)},
                                   {tags,     to_binary(Tags)},
                                   {tweet_id, to_binary(TweetId)}]).

-spec add(string(),string(),params()) -> result_ok() | result_error().
add(ConsumerKey, AccessToken, Query) ->
    case is_valid_param(add, Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(add, Json, json) of
                {ok, Headers, JsonResp} ->
                    {ok, Headers, JsonResp};
                {error, Headers} ->
                    {error, {unable_to_add, Headers}}
            end;
        false ->
            {error, {invalid_add_params, Query}}
    end.

-spec modify(string(),string(),params()) -> result_ok() | result_error().
modify(ConsumerKey, AccessToken, Query) ->
    case is_valid_param(modify, Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(modify, Json, json) of
                {ok, Headers, JsonResp} ->
                    {ok, Headers, JsonResp};
                {error, Headers} ->
                    {error, {unable_to_modify, Headers}}
            end;
        false ->
            {error, {invalid_modify_params, Query}}
    end.

-spec modify(string(),string(),atom(),string()) -> result_ok() | result_error().
modify(ConsumerKey, AccessToken, Action, ItemId) ->
    case modify(ConsumerKey, AccessToken,
           [{actions,
             [{[{action, Action}, {item_id, ItemId}]}]
            }]) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, Headers} ->
            {error, {unable_to_modify, Headers}}
    end.

-spec delete(string(),string(),string()) -> result_ok() | result_error().
delete(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, delete, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_delete, Headers}}
    end.

-spec archive(string(),string(),string()) -> result_ok() | result_error().
archive(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, archive, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_delete, Headers}}
    end.

-spec readd(string(),string(),string()) -> result_ok() | result_error().
readd(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, readd, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_readd, Headers}}
    end.

-spec favorite(string(),string(),string()) -> result_ok() | result_error().
favorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, favorite, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_favorite, Headers}}
    end.

-spec unfavorite(string(),string(),string()) -> result_ok() | result_error().
unfavorite(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, unfavorite, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_unfavorite, Headers}}
    end.

-spec tags_add(string(),string(),string(),list(binary())) ->
                      result_ok() | result_error().
tags_add(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_add, ItemId, Tags) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_tags_add, Headers}}
    end.

-spec tags_remove(string(),string(),string(),list(binary())) ->
                         result_ok() | result_error().
tags_remove(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_remove, ItemId, Tags) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_tags_remove, Headers}}
    end.

-spec tags_replace(string(),string(),string(),list(binary())) ->
                          result_ok() | result_error().
tags_replace(ConsumerKey, AccessToken, ItemId, Tags) ->
    case modify_tags(ConsumerKey, AccessToken, tags_replace, ItemId, Tags) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_tags_replace, Headers}}
    end.

-spec tags_clear(string(),string(),string()) ->
                        result_ok() | result_error().
tags_clear(ConsumerKey, AccessToken, ItemId) ->
    case modify(ConsumerKey, AccessToken, tags_clear, ItemId) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_tags_clear, Headers}}
    end.

-spec tag_rename(string(),string(),string(),string(),string()) ->
                        result_ok() | result_error().
tag_rename(ConsumerKey, AccessToken, ItemId, OldTag, NewTag) ->
    case modify(ConsumerKey, AccessToken,
                [{actions,
                  [{[{action,  tag_rename},
                     {item_id, ItemId},
                     {old_tag, to_binary(OldTag)},
                     {new_tag, to_binary(NewTag)}
                    ]}]
                 }]) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_tags_raname, Headers}}
    end.

-spec is_valid_query(params()) -> boolean().
is_valid_query(Filters) ->
    is_valid_param(retrieve, Filters).

-spec is_valid_param(atom(),params()) -> boolean().
is_valid_param(Type, Filters) ->
    not lists:member(false, [validate_filter(Type, F) || F <- Filters]).

-spec start() -> ok.
start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- ?DEPS],
    ok.


%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_items(ConsumerKey, AccessToken, Filter) ->
    case retrieve(ConsumerKey, AccessToken, Filter) of
        {ok, _Headers, {Response}} ->
            {Items} = proplists:get_value(<<"list">>, Response, {[]}),
            Items;
        Other ->
            maybe_verbose("unable to retrieve items: ~p~n", [Other]),
            []
    end.

get_stats(ConsumerKey, AccessToken, {Type, Params}) ->
    Items = get_items(ConsumerKey, AccessToken,
                      lists:append(Params, [{detailType, simple}])),
    {Type, length(Items)}.

get_json(ConsumerKey, AccessToken, Params) ->
    jiffy:encode({
      lists:append([{consumer_key, to_binary(ConsumerKey)},
                    {access_token, to_binary(AccessToken)}], Params)}).

modify_tags(ConsumerKey, AccessToken, Action, ItemId, Tags) ->
    case modify(ConsumerKey, AccessToken,
                [{actions,
                  [{[{action,  Action},
                     {item_id, ItemId},
                     {tags,    eunsure_binary_list(Tags)}
                    ]}]
                 }]) of
        {ok, Headers, JsonResp} ->
            {ok, Headers, JsonResp};
        {error, {unable_to_modify, Headers}} ->
            {error, {unable_to_modify, Headers}}
    end.

call_api(UrlType, Json, Type) ->
   case http_request(get_url(UrlType), Json) of
       {200, Headers, Response} ->
           {ok, Headers, parse_response(Response, Type)};
       {400, Headers, _} ->
           {error, {missing_required_parameters, Headers}};
       {403, Headers, _} ->
           {error, {invalid_consumer_key, Headers}};
       {_Other, Headers, _Reason} ->
           {error, Headers}
   end.

validate_filter(add, {url, _Value}) ->
    true;
validate_filter(add, {title, _Value}) ->
    true;
validate_filter(add, {tags, _Value}) ->
    true;
validate_filter(add, {tweet_id, _Value}) ->
    true;
validate_filter(retrieve, {state, Value}) ->
    lists:member(Value, [unread, archive, all]);
validate_filter(retrieve, {favorite, Value}) ->
    lists:member(Value, [0, 1]);
validate_filter(retrieve, {tag, Value}) when is_atom(Value) ->
    true;
validate_filter(retrieve, {tag, Value}) when is_binary(Value) ->
    true;
validate_filter(retrieve, {contentType, Value}) ->
    lists:member(Value, [article, image, video]);
validate_filter(retrieve, {sort, Value}) ->
    lists:member(Value, [newest, oldest, title, site]);
validate_filter(retrieve, {detailType, Value}) ->
    lists:member(Value, [complete, simple]);
validate_filter(retrieve, {search, Value}) when is_binary(Value) ->
    true;
validate_filter(retrieve, {domain, Value}) when is_binary(Value) ->
    true;
validate_filter(retrieve, {since, Value})
  when is_integer(Value) andalso Value > 0 ->
    true;
validate_filter(retrieve, {count, Value}) when is_integer(Value) ->
    true;
validate_filter(retrieve, {offset, Value}) when is_integer(Value)->
    true;
validate_filter(modify, {actions, Value}) when is_list(Value) ->
    %TODO: implement validation of nested params
    true;
validate_filter(modify, {action, Value}) when is_atom(Value) ->
    true;
validate_filter(modify, {item_id, _Id}) ->
    true;
validate_filter(_, _) ->
    false.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_binary(Response)).

http_request(Url, Json) ->
    maybe_verbose("call url=~p,json=~p~n", [Url, Json]),
    {ok, {{_, Status, _}, Headers, Response}} =
        httpc:request(post,
                      {Url, ["application/json"], "application/json", Json},
                      [{timeout, infinity}], []),
    {Status, filter_headers(Headers), Response}.

parse_params(Input) ->
    lists:map(fun parse_param/1, string:tokens(Input, "&")).

parse_param(Input) ->
    [Key, Value] = string:tokens(Input, "="),
    {list_to_atom(Key), Value}.

filter_headers(H) ->
    Keys = ["x-error", "x-error-code", "x-limit-key-limit",
            "x-limit-key-remaining", "x-limit-key-reset",
            "x-limit-user-limit", "x-limit-user-remaining",
            "x-limit-user-reset", "x-source"],
    [{K, proplists:get_value(K, H, "")} || K <- Keys].

maybe_verbose(Text, Args) ->
    case application:get_env(erlpocket, verbose, false) of
        true ->
            io:format("erlpocket: " ++ Text, Args);
        false ->
            ignore
    end.

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
