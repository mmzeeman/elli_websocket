

-module(ws_test).

-export([start/0]).


start() -> 
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    WsConfig = [{handler, elli_example_ws}],

    Config = [{mods, [{elli_example_ws, WsConfig}]}],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}, {port, 8000}]).
