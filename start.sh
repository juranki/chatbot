#!/bin/bash

export ERL_LIBS=".:../rabbitmq-erlang-client:../rabbitmq-server"

erl -boot start_sasl -config sys -sname chatbot
#-s chatbot_app