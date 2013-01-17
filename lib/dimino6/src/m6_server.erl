%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, David
%%% @doc
%%% Manages incoming VSIS requests. Implements a parallel TCP server.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_server).
-author("dlapsley@haystack.mit.edu").
-include("main.hrl").
-export([start_link/0, wait_connect/2, init/1, handle_request/3]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Listen_port = case application:get_env(listen_port) of
		      {ok, Port} -> Port;
		      undefined -> ?DEFAULT_LISTEN_PORT
		  end,
    error_logger:info_msg("listen_port ~w~n", [Listen_port]),
    {ok, spawn_link(?MODULE, init, [Listen_port])}.

%%--------------------------------------------------------------------
%% @doc
%% Initializes module.
%%
%% @spec init(ListenPort) -> ignore
%% @end
%%--------------------------------------------------------------------
init(Listen_port) ->
    {ok, Listen_socket} = gen_tcp:listen(Listen_port, ?TCP_OPTIONS),
    Block_size = case application:get_env(block_size) of
		     {ok, Size} -> Size;
		     undefined -> ?DEFAULT_BLOCK_SIZE
		 end,
    wait_connect(Listen_socket, Block_size).

%%--------------------------------------------------------------------
%% @doc
%% Main server processing loop.
%%
%% @spec wait_connect(Listen_port, Block_size) -> {ok, Pid} | ignore
%%                                                 | {error, Error}
%% @end
%%--------------------------------------------------------------------
wait_connect(Listen_socket, Block_size) ->
    {ok, Socket} = gen_tcp:accept(Listen_socket),
    Pid = spawn_link(?MODULE, handle_request, [Socket, [], Block_size]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(Listen_socket, Block_size).

%%--------------------------------------------------------------------
%% @doc
%% Handle incoming request in a seperate micro-process.
%%
%% @spec wait_connect(Socket, List, Block_Size) -> {ok, Pid} | ignore
%%                                                 | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_request(Socket, List, Block_size) ->
    error_logger:info_msg("m6_server:handle_request.~n"),
    case gen_tcp:recv(Socket, 0, Block_size) of
	{ok, Command} ->
	    handle_request(Socket, [Command|List], Block_size),
	    {ok, Reply} = m6_vsis:handle(Command),
	    gen_tcp:send(Socket, Reply);
	Error ->
	    error_logger:info_msg("Recv done: ~p~n", [Error])
    end.
