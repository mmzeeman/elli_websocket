
-type payload() :: binary().

-type elli_websocket_message() ::
	{text, payload()} |
	{binary, payload()} |
	{ping, payload()} |
	{pong, payload()}.

-type elli_websocket_event() :: 
	websocket_open | websocket_close |
	websocket_throw | websocket_error | websocket_exit.