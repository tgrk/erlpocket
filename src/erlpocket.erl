%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket API - http://getpocket.com/developer/docs/
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
         add/6,
         modify/3,

         start/0,
         stop/0
        ]).

-define(DEPS, [sasl, crypto, public_key, ssl, inets, jiffy, erlpocket]).
-define(BASE_URL, "https://getpocket.com/").

%%%============================================================================
%%% API
%%%============================================================================
request_token(ConsumerKey, RedirectUri) ->
    call_api(request_token,
             jiffy:encode(
               {[{consumer_key, to_bin(ConsumerKey)},
                 {redirect_uri, to_bin(RedirectUri)}
                ]}
              ),
             params
            ).

get_authorize_url(Code, RedirectUri) ->
    get_url(authorize_url, Code, RedirectUri).

authorize(ConsumerKey, Code) ->
    call_api(authorize,
             jiffy:encode(
               {[{consumer_key, to_bin(ConsumerKey)},
                 {code, to_bin(Code)}
                ]}
              ),
             params
            ).

retrieve(ConsumerKey, AccessToken, Query) ->
    case is_valid_filter(retrieve, Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(retrieve, Json, json) of
                {ok, JsonResp} ->
                    {ok, JsonResp};
                {Other, Reason} ->
                    {error, {unable_to_get, Other, Reason}}
            end;
        false ->
            throw({invalid_retrieve_params, Query})
    end.

%TODO - evaluate using parallel requests
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

add(ConsumerKey, AccessToken, Url, Title) ->
    add(ConsumerKey,
        AccessToken,
        [{url,      to_bin(Url)},
         {title,    to_bin(Title)}
        ]
       ).

add(ConsumerKey, AccessToken, Url, Title, Tags) ->
    add(ConsumerKey,
        AccessToken,
        [{url,      to_bin(Url)},
         {title,    to_bin(Title)},
         {tags,     to_bin(Tags)}
        ]
       ).

add(ConsumerKey, AccessToken, Url, Title, Tags, TweetId) ->
    add(ConsumerKey,
        AccessToken,
        [{url,      to_bin(Url)},
         {title,    to_bin(Title)},
         {tags,     to_bin(Tags)},
         {tweet_id, to_bin(TweetId)}
        ]
       ).

add(ConsumerKey, AccessToken, Query) ->
    case is_valid_filter(add, Query) of
        true ->
            Json = get_json(ConsumerKey, AccessToken, Query),
            case call_api(add, Json, json) of
                {ok, JsonResp} ->
                    {ok, JsonResp};
                {Other, Reason} ->
                    {error, {unable_to_add, Other, Reason}}
            end;
        false ->
            throw({invalid_add_params, Query})
    end.

modify(ConsumerKey, AccessToken, Params) ->
    Json = get_json(ConsumerKey, AccessToken, Params),
    case call_api(modify, Json, json) of
        {ok, JsonResp} ->
            {ok, JsonResp};
        {Other, Reason} ->
            {error, {unable_to_modify_add, Other, Reason}}
    end.

start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

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
      lists:append([{consumer_key, to_bin(ConsumerKey)},
                    {access_token, to_bin(AccessToken)}
                   ],
                   Params
                  )
    }).

call_api(UrlType, Json, Type) ->
   case http_request(get_url(UrlType), Json) of
        {200, Response} ->
           {ok, parse_response(Response, Type)};
        {400, _} ->
            {error, missing_consumer_key};
        {403, _} ->
            {error, invalid_consumer_key};
        {Other, Reason} ->
            {Other, Reason}
    end.

is_valid_filter(Type, Filters) ->
    not lists:member(
          false, [validate_filter(Type, F) || F <- Filters]
         ).

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
    true;
validate_filter(retrieve, {domain, Value}) when is_list(Value) ->
    true;
validate_filter(retrieve, {since, _Value}) ->
    %TODO: implement timestamp validation
    true;
validate_filter(retrieve, {count, Value}) when is_integer(Value) ->
    true;
validate_filter(retrieve, {offset, Value}) when is_integer(Value)->
    true;
validate_filter(retrieve, {_Type, _Value}) ->
    false.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_bin(Response)).

http_request(Url, Json) ->
    io:format("DEBUG: json=~p~n", [Json]),
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

to_bin(Value) when is_list(Value) ->
    list_to_binary(Value);
to_bin(Value) ->
    Value.
