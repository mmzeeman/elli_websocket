Elli Websocket Handler
======================

elli_websocket is a websocket handler for Elli.

Installation
------------

You can add elli_websocket to your application by adding it as a dependency to your elli 
application. At the moment elli_websocket is dependant on a handover and ssl feature and
needs a specific elli branch.

```erlang
% rebar.config
{deps, [
    {elli, ".*", {git, "git://github.com/mmzeeman/elli.git", {branch, "ssl+handover"}}},
    {elli_websocket, ".*", {git, "git://github.com/mmzeeman/elli_websocket.git", {branch, "master"}}},
    % ...
]}.
```

Afterwards you can run:

```sh
$ ./rebar get-deps
$ ./rebar compile
```


Usage
-----

Callback module
-----------------

```erlang
-module(elli_example_websocket_handler).
-export([websocket_init/1, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

websocket_init(Req, Opts) ->
    State = undefined,
    {ok, [], State}.

websocket_info(Req, Message, State) ->
    {ok, State}.

websocket_handle(Req, Message, State) ->
    {ok, State}.

websocket_terminate(Req, Reason, State) ->
    ok.


```
