-module(elli_example_ws).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([init/2, handle/2, handle_event/3]).

-export([websocket_init/2, websocket_info/3, websocket_handle/3, websocket_terminate/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).
% -behaviour(elli_ws_handler).

%%
%% Elli Handler Callbacks
%%

%% It would be nice if it was possbile to somehow pass the fact
%% that this request is upgraded.
%%
init(Req, Args) ->
    Method = case elli_request:get_header(<<"Upgrade">>, Req) of
        <<"websocket">> ->
            init_ws(elli_request:path(Req), Req, Args);
        _ ->
            ignore
    end.

handle(Req, Args) ->
    Method = case elli_request:get_header(<<"Upgrade">>, Req) of
        <<"websocket">> -> 
            websocket;
        _ ->
            elli_request:method(Req)        
    end,
    handle(Method, elli_request:path(Req), Req, Args).


%%
%%
%%

init_ws([<<"my">>, <<"websocket">>], _Req, _Args) ->
    {ok, handover};
init_ws(_, _, _) ->
    ignore.

handle('websocket', [<<"my">>, <<"websocket">>], Req, Args) ->
    elli_websocket:upgrade(Req, Args),
    {close, <<>>};

handle('GET', [<<"my">>, <<"websocket">>], _Req, _Args) ->
    {200, [], <<"Use an upgrade request">>};

handle(_,_,_,_) ->
    ignore.

handle_event(Name, EventArgs, ElliArgs) ->
    io:fwrite(standard_error, "event: ~p ~p ~p~n", [Name, EventArgs, ElliArgs]),
    ok.


%%
%%
websocket_init(Req, Opts) ->
    io:fwrite(standard_error, "example_ws_init: ~p, ~p ~n", [Req, Opts]),
    State = undefined,
    {ok, [], State}.

websocket_info(Req, Message, State) ->
    io:fwrite(standard_error, "example_ws_info: ~p~n", [Message]),
    {ok, State}.

websocket_handle(Req, Message, State) ->
    io:fwrite(standard_error, "example_ws_handle: ~p~n", [Message]),
    {ok, State}.

websocket_terminate(Req, Reason, State) ->
    io:fwrite(standard_error, "example_ws_terminate: ~p, ~p, ~p~n", [Req, Reason, State]),
    ok.

%%
%% Helper 
%%


    
