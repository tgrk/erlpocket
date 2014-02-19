elrpocket
=========

An Erlang library for Pocket API. For more details see [documentation][1].

## Fetch dependencies

Project depends on [jiffy][3] librariy for JSON parsing.
```
$ rebar get-deps
```

## Compile
```
$ rebar compile
```

## Quick start
```
$ ./start.sh
```

### Authentication
The Pocket API uses custom implementation of oAuth 2.0 for authorization.
This library provide helper functions to authorize your application.

#### Obtain a platform consumer key
First you have to [register][3] your application to get consumer key.

#### Obtain a request token
```erlang
RedirectUri = "http://www.foo.com/",
ConsumerKey = "<app-consumer-key>",
{ok, [{code, Code}]} = erlpocket:request_token(ConsumerKey, RedirectUri).
```
#### Obtain a request token
Use returned security token(code) to get URL that will authorize your
application on Pocket website.
```erlang
Url = erlpocket:get_authorize_url(Code, RedirectUri).
```

#### Convert a request code into Pocket access token
```erlang
{ok, [{access_token, AccessToken},{username, Username}]} = erlpocket:authorize(ConsumerKey, Code).
```

### Working with content API
After sucessfull authentication you are ready call add/modify and retrieve functions.

#### General

You can also get content statistics, but it is just call on top of retrieve so it can take quite some time.
```erlang
Stats = erlpocket:stats(ConsumerKey, AccessToken).
```

Validate API params:
```erlang
true = erlpocket:is_valid_param(add, [{title, "Foobar"}, {url, "http://foobar"}]).
```

#### Retrieve API
To [get][4] content use following call:
```erlang
Query = [{tag, erlang}].
{ok, Response} = erlpocket:retrieve(ConsumerKey, AccessToken, Query).
```
To validate retrieve query there is a helper:
```erlang
true = erlpocket:is_valid_query([{contentType, video}]).
```

#### Add API
[Add][5] new content simply by calling:
```erlang
{ok, {[{<<"item">, _}]}} = erlpocket:add(ConsumerKey, AccessToken, "http://foobar/").
```
or function with different arrity.

#### Modify API
[Update][6] existing content:
```erlang
TODO
```
##### Helpers on top of Modify API
Delete an existing content:
```erlang
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:delete(ConsumerKey, AccessToken, "1234").
```

Archive an existing content:
```erlang
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:archive(ConsumerKey, AccessToken, "1234").
```

Mark an existing content as unread:
```erlang
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:readd(ConsumerKey, AccessToken, "1234").
```

Mark an existing content as favorite:
```erlang
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:favorite(ConsumerKey, AccessToken, "1234").
```

Remove an existing content from favorites:
```erlang
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:unfavorite(ConsumerKey, AccessToken, "1234").
```

Add multiple tags to existing item:
```erlang
Tags = [<<"foo">>, <<"bar">>],
{ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]} = erlpocket:tags_add(ConsumerKey, AccessToken, ItemId, Tags).
````

[1]: http://getpocket.com/developer/docs/overview
[2]: http://getpocket.com/developer/apps/new
[3]: https://github.com/davisp/jiffy
[4]: http://getpocket.com/developer/docs/v3/retrieve
[5]: http://getpocket.com/developer/docs/v3/add
[6]: http://getpocket.com/developer/docs/v3/modify
