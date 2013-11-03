%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%%
%% @doc Elli WebSocket Request Adapter.
%%
%% Copyright 2013 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(elli_ws_request_adapter).

-include_lib("elli/include/elli.hrl").


-export([
	init/2,
	get/2, 
	maybe_reply/2,
	ensure_repsonse/2,
	upgrade_reply/3,
	parse_header/2,
	header/2,
	set_meta/3]).

-export([
	websocket_handler_init/3,
	websocket_handler_callback/5,
	websocket_handler_terminate/4,

	websocket_handler_handle_event/5
	]).

%% Helper function.
-export([messages/1]).

%% Request adapter record.
-record(req_adapter, {
	req,

	%% If set to true we check if we can compress.
	resp_compress = false,

	%% Headers to add to the response.
	resp_headers = [],

	%%
	sent_upgrade_reply = false,

	%% Possible meta values.
	websocket_version=undefined,
	websocket_compress=undefined
	}).

%%
%%
%%

% @doc Initialize the request helper
%
init(#req{}=Req, RespCompress) ->
	#req_adapter{req=Req, resp_compress=RespCompress}.


% @doc Mimics cowboy_req:get/2
%
get(socket, ReqAdapter) ->
	Req = ReqAdapter#req_adapter.req, 
	Req#req.socket;
get(resp_compress, ReqAdapter) ->
	ReqAdapter#req_adapter.resp_compress;
get(L, Req) when is_list(L) ->
	get(L, Req, []).
get([], _Req, Acc) ->
 	lists:reverse(Acc);
get([H|T], Req, Acc) ->
 	get(T, Req, [get(H, Req)|Acc]).


% @doc Mimics cowboy_req:maybe_reply/2
%
maybe_reply(400, ReqAdapter) ->
	case ReqAdapter#req_adapter.sent_upgrade_reply of
		true ->
			already_sent_reply;
		false ->	
			reply(400, ReqAdapter)
	end.


% @doc Mimics cowboy_req:ensure_response/2
%
ensure_repsonse(ReqAdapter, 400) ->
	reply(400, ReqAdapter).


%% 
%% Send an upgrade reply to the 
upgrade_reply(101, Headers, #req_adapter{req=Req}=RA) ->
	UpgradeHeaders = [{<<"Connection">>, <<"Upgrade">>}|Headers],
	elli_http:send_response(Req, 101, RA#req_adapter.resp_headers ++ UpgradeHeaders, <<>>),
	{ok, RA#req_adapter{sent_upgrade_reply=true}}.



%% Note: The headers keys are already parsed by Erlang decode_packet. This
%% means that all keys are capitalized.

% @doc Mimics cowboy_req:parse_header/3 {ok, ParsedHeaders, Req}
%
parse_header(<<"upgrade">>, #req_adapter{req=Req}=RA) ->
	%% case insensitive tokens.
	Values = get_header_values(<<"Upgrade">>, Req),
	{ok, elli_ws_http:tokens(Values), RA};
parse_header(<<"connection">>, #req_adapter{req=Req}=RA) ->
	Values = get_header_values(<<"Connection">>, Req),
	{ok, elli_ws_http:tokens(Values), RA};
parse_header(<<"sec-websocket-extensions">>, #req_adapter{req=Req}=RA) ->
	Values = get_header_values(<<"Sec-WebSocket-Extensions">>, Req),
	%% We only recognize x-webkit-deflate-frame, which has no args,
	%% skip the rest.
	Exts = elli_ws_http:tokens(Values),
	Extensions = [{E, []} || E <- Exts, E =:= <<"x-webkit-deflate-frame">>],
	{ok, Extensions, RA}.

% @doc Mimics cowboy_req:header/2
%
header(<<"sec-websocket-version">>, #req_adapter{req=Req}=RA) ->
	{get_header_value(<<"Sec-WebSocket-Version">>, Req), RA};
header(<<"sec-websocket-key">>, #req_adapter{req=Req}=RA) ->
	{get_header_value(<<"Sec-Websocket-Key">>, Req), RA}.


% @doc Mimics cowboy_req:set_meta/3
% 
set_meta(websocket_version, Version, ReqAdapter) ->
	ReqAdapter#req_adapter{websocket_version = Version};
set_meta(websocket_compress, Bool, ReqAdapter) ->
	ReqAdapter#req_adapter{websocket_compress = Bool}.


%% @doc Initialize the websocket handler.

% @doc Call the websocket_init callback of the websocket handler.
%
% calls websocket_init(Req, HandlerOpts) ->
%     {ok, Headers, HandlerState} - We can upgrade, headers are added to the upgrade response.
%     {ok, Headers, hibernate, HandlerState} - We can upgrade, but this process will hibernate, headers 
%         are added to the upgrade response
%     {ok, Headers, Timeout, HandlerState} - We can upgrade, we will timout, headers are added to the 
%         upgrade respose.
%     {ok, Headers, hibernate, Timeout, HandlerState} - We can upgrade, set a timeout and hibernate. 
%         Headers are added to the response.
%     {shutdown, Headers} - We can't upgrade, a bad request response will be sent to the client.
%
websocket_handler_init(#req_adapter{req=Req}=RA, Handler, HandlerOpts) ->
	case Handler:websocket_init(Req, HandlerOpts) of
		{shutdown, Headers} ->
			{shutdown, RA#req_adapter{resp_headers=Headers}};
		{ok, Headers, HandlerState} ->
			{ok, RA#req_adapter{resp_headers=Headers}, HandlerState};
		{ok, Headers, hibernate, HandlerState} ->
			{ok, RA#req_adapter{resp_headers=Headers}, HandlerState, hibernate};
		{ok, Headers, Timeout, HandlerState} ->
			{ok, RA#req_adapter{resp_headers=Headers}, HandlerState, Timeout};
		{ok, Headers, hibernate, Timeout, HandlerState} ->
			{ok, RA#req_adapter{resp_headers=Headers}, HandlerState, Timeout, hibernate}
	end.

%% @doc Calls websocket_info en websocket_handle callbacks.
websocket_handler_callback(#req_adapter{req=Req}=RA, Handler, Callback, Message, HandlerState) ->
	case Handler:Callback(Req, Message, HandlerState) of 
		{ok, HandlerState1} ->
			{ok, RA, HandlerState1};
		{ok, hibernate, HandlerState1} ->
			{ok, RA, HandlerState1, hibernate};
		{reply, Payload, HandlerState1} ->
			{reply, Payload, RA, HandlerState1};
		{reply, Payload, hibernate, HandlerState1} ->
			{reply, Payload, RA, HandlerState1, hibernate};
		{shutdown, HandlerState1} ->
			{shutdown, RA, HandlerState1}
	end.

%% @doc The websocket is terminated.. call websocket_terminate.
websocket_handler_terminate(#req_adapter{req=Req}, Handler, TerminateReason, HandlerState) ->
	Handler:websocket_terminate(Req, TerminateReason, HandlerState).

%% @doc Report an event...
websocket_handler_handle_event(#req_adapter{req=Req}, Handler, Name, EventArgs, HandlerOpts) ->
    try
        Handler:websocket_handle_event(Name, [Req|EventArgs], HandlerOpts)
    catch
        EvClass:EvError ->
            error_logger:error_msg("~p:handle_event/3 crashed ~p:~p~n~p",
                                   [Handler, EvClass, EvError,
                                    erlang:get_stacktrace()])
    end.

% @doc Atoms used to identify messages in {active, once | true} mode.
messages(#req_adapter{}=RA) ->
	messages(get(socket, RA));
messages({plain, _}) ->
    {tcp, tcp_closed, tcp_error};
messages({ssl, _}) ->
    {ssl, ssl_closed, ssl_error}.

%%
%% Helpers
%%

% @doc Send a bad_request reply.
%
reply(400, #req_adapter{req=Req}) ->
    Body = <<"Bad request">>,
    Size = size(Body),

    elli_http:send_response(Req, 400, [{"Connection", "close"},
                                       {"Content-Length", Size}], Body).

%% Get all header values for Key
get_header_values(Key, #req{headers=Headers}) ->
    elli_proplists:get_all_values_ci(Key, Headers).

% Get the first value.
get_header_value(Key, #req{headers=Headers}) ->
    elli_proplists:get_value_ci(Key, Headers).
