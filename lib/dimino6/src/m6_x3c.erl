%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc 
%%% Handles mapping and sending/retrieving of vsis to x3c
%%% commands.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_x3c).
-compile(export_all).
-author("dlapsley@haystack.mit.edu").
-include("main.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send command to x3c system.
%%
%% @spec send_x3c_cmd(Type, Args) -> {ok, Response}
%%                                   | {error, Error}.
%% @end
%%--------------------------------------------------------------------
% 1 arity
send_x3c_cmd(get_record_status) ->
    gen_xml(get_record_status, []);

send_x3c_cmd(get_recorder_info) ->
    gen_xml(get_recorder_info, []);

send_x3c_cmd(get_system_status) ->
    gen_xml(get_system_status, []).

% 2 arity.
send_x3c_cmd(get_disk_data, V) ->
    gen_xml(get_disk_data,
	    [#param{pname="volumeName", type="string", value=V}]);

send_x3c_cmd(get_disk_performance, V) ->
    gen_xml(get_disk_performance,
	    [#param{pname="volumeName", type="string", value=V}]);

send_x3c_cmd(get_input_stream, S) ->
    gen_xml(get_input_stream,
	    [#param{pname="streamLabel", type="string", value=S}]);

send_x3c_cmd(get_module_serial_number, V) ->
    gen_xml(get_module_serial_number,
	    [#param{pname="volumeName", type="string", value=V}]);

send_x3c_cmd(get_volume_info, V) ->
    gen_xml(get_volume_info,
	    [#param{pname="volumeName", type="string", value=V}]);

send_x3c_cmd(get_volume_state, V) ->
    gen_xml(get_volume_state,
	    [#param{pname="volumeName", type="string", value=V}]);

send_x3c_cmd(get_volume_state_mask, V) ->
    gen_xml(get_volume_state_mask,
	    [#param{pname="volumeName", type="string", value=V}]).

% 3 arity.
send_x3c_cmd(set_module_serial_number, V, M) ->
    gen_xml(set_module_serial_number,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="MSN", type="string", value=M}]);

send_x3c_cmd(set_volume_action, V, A) ->
    gen_xml(set_volume_action,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="action", type="keyword", value=A}]);

send_x3c_cmd(set_volume_state, V, D) ->
    gen_xml(set_volume_state,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="dsmStatus", type="keyword", value=D}]);

send_x3c_cmd(get_volume_usage, V, R) ->
    gen_xml(get_volume_usage,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="recordingSpeed", type="unsigned", value=R}]).

% 4 arity.
send_x3c_cmd(check_data, V, P, I) ->
    gen_xml(check_data,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="projectName", type="string", value=P},
	     #param{pname="interfaceName", type="string", value=I}]);

send_x3c_cmd(define_volume, A, N, V) ->
    gen_xml(define_volume,
	    [#param{pname="action", type="keyword", value=A},
	     #param{pname="newVolumeName", type="string", value=N},
	     #param{pname="volumeName", type="string", value=V}]).

% 5 arity.
send_x3c_cmd(set_volume_state_mask, V, E, RPE, REE) ->
    gen_xml(set_volume_state_mask,
	    [#param{pname="volumeName", type="string", value=V},
	     #param{pname="eraseEnable", type="boolean", value=E},
	     #param{pname="replayEnable", type="boolean", value=RPE},
	     #param{pname="recordEnable", type="boolean", value=REE}]).

% 6 arity.
send_x3c_cmd(set_record, A, S, E, SC, FNS) ->
    gen_xml(set_record,
	    [#param{pname="action", type="keyword", value=A},
	     #param{pname="scanName", type="string", value=S},
	     #param{pname="experimentName", type="string", value=E},
	     #param{pname="stationCode", type="string", value=SC},
	     #param{pname="fileNameSuffix", type="string", value=FNS}]).
% 7 arity.
send_x3c_cmd(set_input_stream, S, D, I, PF, PO, IP) ->
    gen_xml(set_input_stream,
	    [#param{pname="streamLabel", type="string", value=S},
	     #param{pname="dataFormat", type="keyword", value=D},
	     #param{pname="interfaceID", type="string", value=I}
	     #param{pname="packetFilter", type="keyword", value=PF},
	     #param{pname="payloadOffset", type="unsigned", value=PO},
	     #param{pname="IPAddress", type="IPAddress", value=IP}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return supported commands.
%%
%% @spec commands() -> list()
%% @end
%%--------------------------------------------------------------------
commands() ->
    [
     define_volume,
     set_input_stream,
     set_module_serial_number,
     set_record,
     set_volume_action,
     set_volume_state,
     set_volume_state_mask
     ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return supported queries.
%%
%% @spec queries() -> list()
%% @end
%%--------------------------------------------------------------------
queries() ->
    [
     check_data,
     get_disk_data,
     get_disk_performance,
     get_input_stream,
     get_module_serial_number,
     get_record_status,
     get_recorder_info,
     get_system_status,
     get_volume_info,
     get_volume_state,
     get_volume_state_mask,
     get_volume_usage
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the XML representatino of a command and its param list.
%%
%% @spec gen_xml(Command, Param_list) -> string()
%% @end
%%--------------------------------------------------------------------
gen_xml(Command, Param_list) ->
    Content = {x3c_cmd, [{name, [Command]}, {params, gen_xml(Param_list)}]},
    lists:flatten(xmerl:export_simple([ Content  ], xmerl_xml,
				      [{prolog, ?XML_PROLOG}])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a parameter list into form for input into xmerl.
%% Parameter list is  a list of parameter records.
%%
%% @spec gen_xml(Param_list) -> string()
%% @end
%%--------------------------------------------------------------------
gen_xml(Param_list) ->
    lists:map(fun(X) ->
		      {param, [{pname, [X#param.pname]},
			       {type, [X#param.type]},
			       {value, [X#param.value]}]}
	      end, Param_list).
