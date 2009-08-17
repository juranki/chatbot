%%%-------------------------------------------------------------------
%%% File    : chatbot_srv_tx.erl
%%% @author  <juhani@juranki.com>
%%% @copyright  2009 <juhani@juranki.com>
%%% @doc Added transactions and acknowledgements to `chatbot_srv'
%%% @end
%%%
%%% Created : 17 Aug 2009 by  <juhani@juranki.com>
%%%-------------------------------------------------------------------
-module(tx_chatbot_srv).

-behaviour(gen_server).

-include_lib("rabbit.hrl").
-include_lib("rabbit_framing.hrl").
-include_lib("amqp_client.hrl").


%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection, channel, q_name, c_tag}).

-define(SERVER,?MODULE).

%% @spec (string(),integer(),binary(),binary(),binary()) -> Result
%%         Result = {ok,pid()} | ignore | {error,Error}
%%          Error = {already_started,pid()} | term()
start_link(Host,Port,Uid,Pwd,VHost) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host,Port,Uid,Pwd,VHost], []).


%% @doc Initialize. Open AMQP Connection and Channel.
%% Declare queue and bind it to an exchange. Start consuming messages from the queue.
%%
%% @spec ([term()]) -> Result
%%        Result = {ok,term ()} | ignore | {error, term()}
init([Host,Port,Uid,Pwd,VHost]) ->
    error_logger:info_report([init,{vhost,VHost}]),
    Connection = amqp_connection:start_network_link(
                   #amqp_params{username = Uid, 
                                password = Pwd, 
                                virtual_host = VHost,
                                host = Host,
                                port = Port}),
    Channel = amqp_connection:open_channel(Connection),

    #'queue.declare_ok'{queue = QName} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true,
                                                    auto_delete = true}),

    #'basic.consume_ok'{consumer_tag = ConsumerTag} =
        amqp_channel:subscribe(Channel, 
                               #'basic.consume'{queue = QName,
                                                no_ack = false}, 
                               self()),
    
    #'queue.bind_ok'{} = 
        amqp_channel:call(Channel, #'queue.bind'{queue = QName, 
                                                 exchange = <<"rabbit">>,
                                                 routing_key = <<>>}),
    
    ok = tx_select(Channel),

    {ok, #state{connection = Connection,
                channel = Channel,
                q_name = QName,
                c_tag = ConsumerTag}}.



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% @doc In gen_server handle_info/2 receives the subscribed messages
%%
%% @spec (InfoData::Data, #state{}) -> {noreply, #state{}}
%%        Data = Message | ConsumeOK | term()
%%         Message = {#'basic.deliver'{},#amqp_msg{}}
%%         ConsumeOK = #'basic.consume_ok'{}
handle_info(#'basic.consume_ok'{consumer_tag=Tag}, State) ->
    {noreply, State#state{c_tag=Tag}};
handle_info({#'basic.deliver'{consumer_tag=Tag1,
                              routing_key=RK,
                              delivery_tag=DeliveryTag},
             #amqp_msg{props = #'P_basic'{content_type= <<"text/plain">>},
                       payload = Payload}},
            State = #state{c_tag = Tag2, 
                           channel=Channel}) 
  when Tag1 =:= Tag2, RK =/= <<"chatbot">>, RK =/= <<"enterpricey_chatbot">> ->
    publish_plain_text(Channel, <<"rabbit">>, <<"enterpricey_chatbot">>, 
                       <<Payload/binary, " is ok, but I prefer it with transactions and acknowledgements.">>),
    error_logger:info_report(published),
    basic_ack(Channel,DeliveryTag),
    error_logger:info_report(acked),
    ok = tx_commit(Channel),
    error_logger:info_report(commited),
    {noreply,State};
handle_info(Info,State) ->
    error_logger:info_report([chatbot_srv_info,
                              {info,Info}]),
    {noreply, State}.


terminate(_Reason, #state{connection=Connection,channel=Channel}) ->
    #'channel.close_ok'{} = 
        amqp_channel:call(Channel, 
                          #'channel.close'{reply_code = 200,
                                           reply_text = <<"Goodbye">>,
                                           class_id = 0,
                                           method_id = 0}),
    #'connection.close_ok'{} = 
        amqp_connection:close(Connection,
                              #'connection.close'{reply_code = 200,
                                                  reply_text = <<"Goodbye">>,
                                                  class_id = 0,
                                                  method_id = 0}),
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
    Content = #amqp_msg{props = Properties, payload = Payload},
    amqp_channel:cast(Channel, BasicPublish, Content).


tx_select(Channel) ->
    #'tx.select_ok'{} = amqp_channel:call(Channel, #'tx.select'{}),
    ok.

tx_commit(Channel) ->
    #'tx.commit_ok'{} = amqp_channel:call(Channel, #'tx.commit'{}),
    ok.

basic_ack(Channel,DeliveryTag) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag=DeliveryTag}).
