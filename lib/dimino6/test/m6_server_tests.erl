%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_server_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 500).
-define(SETTLING_DELAY_MS, 500).

start_server() ->
    error_logger:info_msg("m6_server_test:start_server()~n"),
    m6_server:start_link().

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(connect())},
     {timeout, ?TEST_TIMEOUT_MS, ?_test(send())}
     ].

connect() ->
    error_logger:info_msg("m6_server_test:connect_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    {ok, Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
				 [list, {packet, 0}]),
    error_logger:info_msg("m6_server_test:connect_test() sleeping~n"),
    timer:sleep(1000),
    exit(Pid, "Finished test."),
    ok.

send() ->
    error_logger:info_msg("m6_server_test:send_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    {ok, Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
			 [list, {packet, 0}]),
    timer:sleep(?SETTLING_DELAY_MS),
    gen_tcp:send(Sock, "This is so cool"),
    gen_tcp:close(Sock),
    timer:sleep(?SETTLING_DELAY_MS),
    exit(Pid, "Finished test."),
    ok.

send_command_list([H|T]) ->
    {ok, Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
				 [list, {packet, 0}]),
    gen_tcp:send(Sock, H),
    gen_tcp:close(Sock),
    timer:sleep(?INTER_COMMAND_DELAY_MS),
    send_command_list(T);
send_command_list([]) ->
    ok.
