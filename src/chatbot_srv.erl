%%%-------------------------------------------------------------------
%%% File    : chatbot_srv.erl
%%% Author  :  <juhani@juranki.com>
%%% Description : 
%%%
%%% Created : 30 Jul 2009 by  <juhani@juranki.com>
%%%-------------------------------------------------------------------
-module(chatbot_srv).

-include("rabbit.hrl").
-include_lib("rabbit_framing.hrl").

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection, channel, q_name, c_tag}).

-define(SERVER,?MODULE).


start_link(QHost,QPort,QUid,QPwd,VHost) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [QHost,QPort,QUid,QPwd,VHost], []).



init([QHost,QPort,QUid,QPwd,VHost]) ->
    error_logger:info_report([init,{vhost,VHost}]),
    Connection = amqp_connection:start_network_link(QUid, QPwd, QHost,QPort,VHost),
    Channel = amqp_connection:open_channel(Connection),
    QName = lib_amqp:declare_private_queue(Channel),

    lib_amqp:subscribe(Channel, QName, self(), <<"">>),

    lib_amqp:bind_queue(Channel,<<"rabbit">>,QName,<<"">>),

    {ok, #state{connection = Connection,
                channel = Channel,
                q_name = QName}}.



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(#'basic.consume_ok'{consumer_tag=Tag}, State) ->
    {noreply, State#state{c_tag=Tag}};
handle_info({#'basic.deliver'{consumer_tag=Tag1,
                              routing_key=RK},
             #content{payload_fragments_rev = [Payload|_IgnoreRest]}},
            State = #state{c_tag = Tag2, 
                           channel=Channel}) 
  when Tag1 =:= Tag2, RK =/= <<"chatbot">> ->
    publish_plain_text(Channel, <<"rabbit">>, <<"chatbot">>, 
                       <<"Hello, ", RK/binary, ": ", 
                        Payload/binary, " to you too">>),
    {noreply,State};
handle_info(Info,State) ->
    error_logger:info_report([chatbot_srv_info,
                              {info,Info}]),
    {noreply, State}.


terminate(_Reason, #state{connection=Connection,channel=Channel}) ->
    lib_amqp:teardown(Connection,Channel),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

publish_plain_text(Channel, X, RoutingKey, Payload) ->
    Properties = #'P_basic'{content_type = <<"text/plain">>,
                            delivery_mode = 1,
                            priority = 0},
    BasicPublish = #'basic.publish'{exchange = X,
                                    routing_key = RoutingKey,
                                    mandatory = false},
    {ClassId, _MethodId} = rabbit_framing:method_id('basic.publish'),
    Content = #content{class_id = ClassId,
                       properties = Properties,
                       properties_bin = none,
                       payload_fragments_rev = [Payload]},
    amqp_channel:cast(Channel, BasicPublish, Content).
