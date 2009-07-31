{application, chatbot,
 [{description, "Toy app for learning amqp/rabbitmq. Echoes back what you type to the chat demo"},
  {vsn, "0.1"},
  {modules, [chatbot_app, chatbot_sup, chatbot_srv]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {env, [{server,"127.0.0.1"},
         {port, 5672},
         {uid,"guest"},
         {pwd,"guest"},
         {vhost,<<"/">>}]},
  {mod, {chatbot_app,[]}}
 ]}.
