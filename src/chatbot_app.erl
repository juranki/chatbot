%%%-------------------------------------------------------------------
%%% File    : chatbot_app.erl
%%% Author  :  <juhani@juranki.com>
%%% Description : 
%%%
%%% Created : 30 Jul 2009 by  <juhani@juranki.com>
%%%-------------------------------------------------------------------
-module(chatbot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
    application:start(chatbot).

start(_Type, _StartArgs) ->
    chatbot_sup:start_link().

stop(_State) ->
    ok.
