%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang librafy for Pocket API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2013 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erlpocket).

%% API
-export([
         request_token/2,
         get_authorize_url/2,
         authorize/2,

         get_stats/2,

         get_unreaded/3,
         get_archive/3,
         get_favourite/3,
         get_by_tag/4,
         get/3,

         add/6,
         add/3,


         start/0
        ]).

-define(APP, erlpocket).
-define(BASE_URL, "https://getpocket.com/").


%%%============================================================================
%%% API
%%%============================================================================
request_token(ConsumerKey, RedirectUri) ->
    call_api(
      get_url(request_token),
      jiffy:encode({[{consumer_key, to_bin(ConsumerKey)},
                     {redirect_uri, to_bin(RedirectUri)}
                    ]}),
      params
     ).

get_authorize_url(Token, RedirectUri) ->
    get_url(authorize_url, Token, RedirectUri).

authorize(ConsumerKey, Code) ->
    call_api(
      get_url(authorize),
      jiffy:encode({[{consumer_key, to_bin(ConsumerKey)},
                     {code, to_bin(Code)}
                    ]}),
      params
     ).

get_unreaded(ConsumerKey, AccessToken, Query) ->
    get(ConsumerKey,
        AccessToken,
        lists:append([{state, <<"unread">>}], Query)
       ).

get_archive(ConsumerKey, AccessToken, Query) ->
    get(ConsumerKey,
        AccessToken,
        lists:append([{state, <<"archive">>}], Query)
       ).

get_favourite(ConsumerKey, AccessToken, Query) ->
    get(ConsumerKey,
        AccessToken,
        lists:append([{favourite, <<"1">>}], Query)
       ).

get_stats(ConsumerKey, AccessToken) ->
    All = get_items(ConsumerKey, AccessToken, [{state, <<"all">>}]),
    Output = [{total_archive,  <<"state">>, <<"archive">>},
              {total_favorites, <<"favorite">>, <<"1">>},
              {total_acticles, <<"is_article">>, <<"0">>},
              {total_videos, <<"has_video">>, <<"0">>}
             ],
    lists:append(
      [
       [{total_items, length(All)}],
       filter_stats_items(All, Output)
      ]
     ).

get_by_tag(ConsumerKey, AccessToken, TagName, Query) ->
    get(ConsumerKey,
        AccessToken,
        lists:append([{tag, TagName}], Query)
       ).

get(ConsumerKey, AccessToken, Query) ->
    Json = get_json(ConsumerKey, AccessToken, Query),
    case call_api(get_url(retrieve), Json, json) of
        {ok, JsonResp} -> JsonResp;
        {Other, Reason} ->
            {error, {unable_to_get, Other, Reason}}
    end.

add(ConsumerKey, AccessToken, Url, Title, Tags, TweetId) ->
    Query = [{url,      to_bin(Url)},
             {title,    to_bin(Title)},
             {tags,     to_bin(Tags)},
             {tweet_id, to_bin(TweetId)}
            ],
    add(ConsumerKey, AccessToken, Query).

add(ConsumerKey, AccessToken, Query) ->
    Json = get_json(ConsumerKey, AccessToken, Query),
    case call_api(get_url(add), Json, json) of
        {ok, JsonResp} -> JsonResp;
        {Other, Reason} ->
            {error, {unable_to_add, Other, Reason}}
    end.


start() ->
    [application:start(A) || A <- deps() ++ [?APP]],
    ok.


%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_items(ConsumerKey, AccessToken, Filter) ->
    {[{<<"status">>,1},
      {<<"complete">>,1},
      {<<"list">>, {Items}},
      {<<"since">>, _Since}
     ]} = get(ConsumerKey, AccessToken, Filter),
    Items.

filter_stats_items(All, Output) ->
    lists:map(
      fun({Name, Filter}) ->
              {Name, get_filter_item_count(All, Filter)}
      end, Output).

get_filter_item_count(Items, {FilterName, FilterValue}) ->
    length(
      lists:filter(
        fun({_Id, {List}}) ->
                case proplists:get_value(FilterName, List) of
                    FilterValue -> false;
                    _           -> true
                end
        end, Items)
     ).

get_json(ConsumerKey, AccessToken, Params) ->
    jiffy:encode({
      lists:append([
                    {consumer_key, to_bin(ConsumerKey)},
                    {access_token, to_bin(AccessToken)}
                   ],
                   Params
                  )
    }).

call_api(Url, Json, Type) ->
   case http_request(Url, Json) of
        {200, Response} ->
           {ok, parse_response(Response, Type)};
        {400, _} ->
            {error, missing_consumer_key};
        {403, _} ->
            {error, invalid_consumer_key};
        {Other, Reason} ->
            {Other, Reason}
    end.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_bin(Response)).

http_request(Url, Json) ->
    {ok, {{_, Status, _}, _, Response}} =
        httpc:request(
            post,
            {Url, ["application/json"], "application/json", Json},
            [], []
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
    ?BASE_URL ++ "v3/add".

get_url(authorize_url, Code, RedirectUri) ->
    ?BASE_URL ++ "auth/authorize?request_token=" ++ Code ++ ""
        "&redirect_uri=" ++ RedirectUri.

to_bin(Value) ->
    list_to_binary(Value).

deps() ->
    [crypto, public_key, ssl, inets, jiffy].
