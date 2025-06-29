[![CircleCI](https://circleci.com/gh/tgrk/erlpocket/tree/master.svg?style=svg)](https://circleci.com/gh/tgrk/erlpocket/tree/master)
[![Hex pm](http://img.shields.io/hexpm/v/erlpocket.svg?style=flat)](https://hex.pm/packages/erlpocket)
[![codecov.io](https://codecov.io/github/tgrk/erlpocket/coverage.svg?branch=master)](https://codecov.io/github/tgrk/erlpocket?branch=master)

## **This project is archived as Mozilla is shutting down Pocket (read more details [here](https://getpocket.com/farewell)).**

# erlpocket

An Erlang library for Pocket API (www.getpocket.com) v3. For more details see [documentation][1].

## Fetch dependencies and compile

Project depends on [jiffy][3] library for JSON parsing and uses default HTTP client (httpc).

    $ rebar3 update compile

or

    $ rebar get-deps compile

## Changes with version 2.x.x

New version breaks backward compatiblity due to replacing old jiffy style proplists with maps. This should be more convenient to
use with latest versions of Erlang. Another big change is that API favors binary input over string input eg. for url when calling add API.

## Quick start

To run all required dependencies start with:

```erlang
_ = application:ensure_all_started(erlpocket).
```

View the [Error and Response Headers Documentation][7] for detailed information about API errors. Also check the [Rate Limits][8] of API.

### Authentication

The Pocket API uses custom implementation of oAuth 2.0 for authentiaction.
This library provide helper functions to authorize your application.

#### Obtain a platform consumer key

First you have to [register][3] your application to get consumer key.

#### Obtain a request token

```erlang
RedirectUri = <<"http://www.foo.com/">>,
ConsumerKey = <<"app-consumer-key">>,
{ok, #{code := Code}} = erlpocket:request_token(ConsumerKey, RedirectUri).
```

Use returned security token (code) to get URL that will authorize your
application on Pocket website.

```erlang
Url = erlpocket:get_authorize_url(Code, RedirectUri).
```

#### Convert a request code into Pocket access token

```erlang
{ok, #{access_token := AccessToken, username := Username}} = erlpocket:authorize(ConsumerKey, Code).
```

### Working with content API

After sucessfull authentication you are ready call add/modify and retrieve functions.

#### General

You can also get content statistics, but it is just call on top of retrieve so it can take quite some time.

```erlang
{ok, Stats} = erlpocket:stats(ConsumerKey, AccessToken).
```

Validate API params:

```erlang
true = erlpocket:is_valid_param(add, #{title => <<"Foobar">>, url => <<"http://foobar">>}).
true = erlpocket:is_valid_param(retrieve, #{tag => <<"Foobar">>}).
```

#### Retrieve API

To [get][4] content use following call:

```erlang
{ok, Response} = erlpocket:retrieve(ConsumerKey, AccessToken, #{tag => <<"erlang">>}).
```

To validate retrieve query there is a helper:

```erlang
true = erlpocket:is_valid_query(#{contentType => video}).
```

#### Add API

[Add][5] new content simply by calling:

```erlang
{ok, #{<<"item"> := _}} = erlpocket:add(ConsumerKey, AccessToken, <<"http://foobar/">>).
```

or function with different arrity.

#### Modify API

[Update][6] existing content. There are multiple helpers to ease work with this API call.

##### Helpers on top of Modify API

Delete an existing content:

```erlang
ItemId = "123",
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:delete(ConsumerKey, AccessToken, ItemId).
```

Archive an existing content:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:archive(ConsumerKey, AccessToken, ItemId).
```

Mark an existing content as unread:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:readd(ConsumerKey, AccessToken, ItemId).
```

Mark an existing content as favorite:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:favorite(ConsumerKey, AccessToken, ItemId).
```

Remove an existing content from favorites:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:unfavorite(ConsumerKey, AccessToken, ItemId).
```

Add multiple tags to existing item:

```erlang
ItemId = <<"123">>,
Tags = [<<"foo">>, <<"bar">>],
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:tags_add(ConsumerKey, AccessToken, ItemId, Tags).
```

Remove multiple tags from an existing item:

```erlang
ItemId = "123",
Tags = [<<"foo">>],
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:tags_remove(ConsumerKey, AccessToken, ItemId, Tags).
```

Replace multiple tags from an existing item:

```erlang
ItemId = <<"123">>,
NewTags = [<<"foo1">>, <<"bar1">>],
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:tags_replace(ConsumerKey, AccessToken, ItemId, NewTags).
```

Remove multiple tags from an existing item:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:tags_clear(ConsumerKey, AccessToken, ItemId).
```

Rename tag of an existing item:

```erlang
ItemId = <<"123">>,
{ok, #{<<"action_results">> := [true], <<"status">> := 1}} = erlpocket:tag_rename(ConsumerKey, AccessToken, ItemId, <<"foo">>, <<"foo1">>).
```

## Example

For example usage of this library please refer to tests. Note that tests are are using real API endpoints (copy template `api.sample.txt` as `api.txt` and fill your Pocket Appplication creadentials there).

[1]: http://getpocket.com/developer/docs/overview
[2]: http://getpocket.com/developer/apps/new
[3]: https://github.com/davisp/jiffy
[4]: http://getpocket.com/developer/docs/v3/retrieve
[5]: http://getpocket.com/developer/docs/v3/add
[6]: http://getpocket.com/developer/docs/v3/modify
[7]: http://getpocket.com/developer/docs/errors
[8]: http://getpocket.com/developer/docs/rate-limits
