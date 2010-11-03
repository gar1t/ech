-module(ech_httpd).

-export([start/0, stop/0]).

-define(HTTPD_SUPPORT, [couch_httpd, couch_uuids]).

start() ->
    lists:foreach(fun ech_sup:start_service/1, ?HTTPD_SUPPORT).

stop() ->
    lists:foreach(fun ech_sup:stop_service/1, lists:reverse(?HTTPD_SUPPORT)).
