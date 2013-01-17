%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(x3c_dummy_server).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").
-export([start/1, server/2, handle_request/3]).


start(Responses) ->
    {ok, spawn(?MODULE, server, [?DEFAULT_XCUBE_PORT, Responses])}.

server(Listen_port, Responses) ->
    {ok, Listen_socket} = gen_tcp:listen(Listen_port, ?TCP_OPTIONS),
    wait_connect(Listen_socket, Responses).

wait_connect(Listen_socket, [H|T]) ->
    {ok, Socket} = gen_tcp:accept(Listen_socket),
    Pid = spawn(?MODULE, handle_request, [Socket, [], H]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(Listen_socket, T);
wait_connect(Listen_socket, []) ->
    error_logger:info_msg("Completed commands, exiting~n").

handle_request(Socket, List, R) ->
    Block_size = case application:get_env(block_size) of
		     {ok, Size} -> Size;
		     undefined -> ?DEFAULT_BLOCK_SIZE
		 end,
    case gen_tcp:recv(Socket, 0, Block_size) of
	{ok, Command} ->
	    io:format("R: ~p~n", [R]),
	    Xml = rsp_xml(R#x3c_resp.name, R#x3c_resp.retval, R#x3c_resp.reason,
			  R#x3c_resp.params),
	    gen_tcp:send(Socket, Xml);
	Done ->
	    error_logger:info_msg("recv done: ~p~n", [Done])
    end,
    gen_tcp:close(Socket).



rsp_xml(Name, Retval, Reason, Param_list) ->
    Content = {x3c_resp, [{name, [Name]},
			 {retval, [Retval]},
			 {reason, [Reason]},
			 {params, rsp_xml(Param_list)}]},
    lists:flatten(xmerl:export_simple([ Content  ], xmerl_xml,
				      [{prolog, ?XML_PROLOG}])).
    
rsp_xml(Param_list) ->
    lists:map(fun(X) ->
		      {param, [{pname, [X#param.pname]},
			       {type, [X#param.type]},
			       {value, [X#param.value]}]}
	      end, Param_list).
