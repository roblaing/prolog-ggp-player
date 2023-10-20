:- use_module([ library(http/http_unix_daemon)
              , library(http/http_server)
              , message_broker
              ]).

/**
 * ```sh
 * swipl server.pl --port=3000 --pidfile=http.pid --sighup=quit
 * kill -9 $(cat http.pid)
 * ```
 **/

:- initialization http_daemon.
:- http_handler(/, move_handler, []).

move_handler(Request) :-
  http_read_json_dict(Request, DictIn),
  message_broker(DictIn, DictOut),
  reply_json_dict(DictOut).

