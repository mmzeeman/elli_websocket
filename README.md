Elli Websocket Handler
======================

elli_websocket is a websocket handler for Elli.

Installation
------------

Usage
-----

Callback moodule
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
