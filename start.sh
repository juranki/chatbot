#!/bin/bash

export ERL_LIBS=".:../rabbitmq-erlang-client:../rabbitmq-erlang-client/deps:../rabbitmq-erlang-client/deps/rabbit_common"

erl -boot start_sasl -config sys -sname chatbot
#-s chatbot_app