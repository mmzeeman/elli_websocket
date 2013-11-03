
-module(elli_websocket_handler).

-include_lib("elli/include/elli.hrl").
-include_lib("elli_websocket.hrl").

-callback websocket_init(Req :: #req{}, Args :: any()) ->
	%% TODO
	any().

-callback websocket_handle(Req :: #req{}, elli_websocket_message(), Args :: any()) ->
	%% TODO
	any().

-callback websocket_info(Req :: #req{}, any(), Args :: any()) ->
	%% TODO
	any().

-callback websocket_handle_event(Event :: elli_websocket_event(), Args :: [tuple()], Config :: [tuple()]) -> 
	ok.
