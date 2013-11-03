%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012, 2013 Maas-Maarten Zeeman
%%
%% @doc Elli Websocket Handler 
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

-module(elli_websocket).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("elli/include/elli.hrl").

-export([upgrade/2]).

% @doc Upgrade the request to a websocket request. 
%    Args: proplist with settings.
%            handler: Websocket callback module
%            handler_opts: Options to pass to the websocket_init 
%                callback.
%            resp_compress: bool(), when set to true the traffic 
%                will be compressed if the client supports it.
%
upgrade(Req, Args) ->
    RespCompress = proplists:get_value(resp_compress, Args, false),
    ReqAdapter = elli_ws_request_adapter:init(Req, RespCompress),
    {handler, Handler} = proplists:lookup(handler, Args),
    HandlerOpts = proplists:get_value(handler_opts, Args, []),

    %% Adapter is ready, hand over to ws_protocol
    elli_ws_protocol:upgrade(ReqAdapter, Args, Handler, HandlerOpts),
    ok.

