%%%-------------------------------------------------------------------
%%% File    : chatbot_app.erl
%%% @author  <juhani@juranki.com>
%%% @copyright  2009 <juhani@juranki.com>
%%% @doc OTP application module for chatbot
%%% @end
%%%
%%% Created : 30 Jul 2009 by  <juhani@juranki.com>
%%%-------------------------------------------------------------------
-module(chatbot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% @doc Shorthand for `application:start(chatbot)'.
start() ->
    application:start(chatbot).

start(_Type, _StartArgs) ->
    chatbot_sup:start_link().

stop(_State) ->
    ok.
