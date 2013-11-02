%% @doc: Elli websocket handler.
%%

-module(elli_websocket).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("elli/include/elli.hrl").

-export([upgrade/2]).


% @doc 
%
upgrade(Req, Args) ->
	RespCompress = proplists:get_value(resp_compress, Args, false),
	ReqAdapter = elli_ws_request_adapter:init(Req, RespCompress),
	{handler, Handler} = proplists:lookup(handler, Args),
	HandlerOpts = proplists:get_value(handler_opts, Args, []),

	%% Adapter is ready, hand over to ws_protocol
	elli_ws_protocol:upgrade(ReqAdapter, Args, Handler, HandlerOpts),
    ok.


%%
%% TESTS
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

config() ->
    [
        {callback, elli_middleware},
        {callback_args,
        [{mods, [
            {elli_ws_example, []},
            {elli_example_callback, []}
        ]}]}
    ].

elli_ws_test() ->
	ok.

-endif.
