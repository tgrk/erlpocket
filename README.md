elrpocket
=========

An Erlang library for Pocket API

## Fetch dependencies

Project depends on [jiffy][2] and [reloader][3] libraries.
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
First you have to [register][1] your application to get consumer key.

#### Obtain a request token
```erlang
RedirectUri = "http://www.foo.com/",
ConsumerKey = "<app-consumer-key>",
{ok, [{code, Code}]} = erlpocket:retrieve(ConsumerKey, RedirectUri).
```
#### Obtain a request token
Use returned security token(code) to get URL that will authorize your
application on Pocket website.
```erlang
Url = erlpocket:get_authorize_url(Code, RedirectUri)
```

#### Convert a request code into Pocket access token
```erlang
{ok, [{access_token, AccessToken},{username, Username}]} = erlpocket:authorize(ConsumerKey, Code)
```
TODO: rest

[1]: http://getpocket.com/developer/apps/new
[2]: https://github.com/davisp/jiffy
[3]: https://github.com/bjnortier/reloader