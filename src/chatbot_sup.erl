%%%-------------------------------------------------------------------
%%% File    : chatbot_sup.erl
%%% Author  :  <juhani@juranki.com>
%%% Description : 
%%%
%%% Created : 30 Jul 2009 by  <juhani@juranki.com>
%%%-------------------------------------------------------------------
-module(chatbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    {ok, QServer} = application:get_env(chatbot, server),
    {ok, QPort} = application:get_env(chatbot, port),
    {ok, QUid} = application:get_env(chatbot, uid),
    {ok, QPwd} = application:get_env(chatbot, pwd),
    Chatbot = {chatbot_srv,
               {chatbot_srv,start_link,[QServer,
                                        QPort,
                                        QUid,
                                        QPwd]},
               permanent,
               2000,
               worker,
               [chatbot_srv]},
    {ok,{{one_for_one,0,1}, [Chatbot]}}.

