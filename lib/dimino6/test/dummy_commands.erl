%%%-------------------------------------------------------------------
%%% @Author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%% This module contains code that facilitates the generation of valid
%%% sequences of VSI-S and X3C commands and responses. It is used for
%%% module and e2e testing.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(dummy_commands).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").
-compile(export_all).

gen_test(N)->
    gen_test(N, [], []).

gen_test(N, C, R) when N>0 ->
    S = length(command_response_mapping()),
    I = random:uniform(S),
    {NewC, NewR} = lists:nth(I, command_response_mapping()),
    gen_test(N-1, [NewC|C], [NewR|R]);
gen_test(0, C, R) ->
    {C, R}.

command_response_mapping() ->
    [
     {vsis_cmd_disk_state(),      x3c_resp_set_volume_state()},
     {vsis_cmd_disk_state_mask(), x3c_resp_set_volume_state_mask()},
     {vsis_cmd_input_stream(),    x3c_resp_set_input_stream()},
     {vsis_cmd_msn(),             x3c_resp_set_module_serial_number()},
     {vsis_cmd_record(),          x3c_resp_set_record()},
     {vsis_cmd_vol_def(),         x3c_resp_define_volume()},
     {vsis_cmd_vol_action(),      x3c_resp_set_volume_action()},
     {vsis_cmd_disk_state(),      x3c_resp_get_volume_state()},
     {vsis_cmd_disk_state_mask(), x3c_resp_set_volume_action()},
     {vsis_qry_disk_state(),      x3c_resp_get_volume_state()},
     {vsis_qry_disk_state_mask(), x3c_resp_get_volume_state_mask()},
     {vsis_qry_input_stream(),    x3c_resp_get_input_stream()},
     {vsis_qry_msn(),             x3c_resp_get_module_serial_number()},
     {vsis_qry_record(),          x3c_resp_get_record_status()},
     {vsis_qry_vol_def(),         x3c_resp_get_volume_info()},
     {vsis_qry_data_check(),      x3c_resp_check_data()},
     {vsis_qry_disk_model(),      x3c_resp_get_disk_data()},
     {vsis_qry_disk_serial(),     x3c_resp_get_disk_data()},
     {vsis_qry_disk_size(),       x3c_resp_get_disk_data()},
     {vsis_qry_disk_perf(),       x3c_resp_get_disk_performance()},
     {vsis_qry_dts_id(),          x3c_resp_get_recorder_info()},
     {vsis_qry_status(),          x3c_resp_get_system_status()},
     {vsis_qry_rtime(),           x3c_resp_get_volume_usage()},
     {vsis_qry_vol_usage(),       x3c_resp_get_volume_usage()}
    ].

vsis_cmd_disk_state() ->
    "disk_state=VOL1:ACTIVE;".

vsis_cmd_disk_state_mask() ->
    "disk_state_mask=VOLUME_NAME:ERASE_ENABLE:REPLAY_ENABLE:RECORD_ENABLE;".

vsis_cmd_input_stream() ->
    "input_stream=STREAM_LABEL:DATA_FORMAT:INTERFACE_ID:PACKET_FILTER:PAYLOAD_OFFSET:IP_ADDRES;".

vsis_cmd_msn()->
    "msn=VOLUME_NAME:MSN;".

vsis_cmd_record()->
    "record=ACTION:SCAN_NAME:EXPERIMENT_NAME:STATION_CODE:FILE_NAME_SUFFIX;".

vsis_cmd_vol_def()->
    "vol_def=ACTION:NEW_VOLUME_NAME:VOLUME_NAME;".

vsis_cmd_vol_action()->
    "vol_action=VOLUME_NAME:ACTION;".

vsis_qry_disk_state()->
    "disk_state?VOLUME_NAME;".

vsis_qry_disk_state_mask()->
    "disk_state_mask?VOLUME_NAME;".

vsis_qry_input_stream()->
    "input_stream?STREAM_LABEL;".

vsis_qry_msn()->
    "msn?VOLUME_NAME;".

vsis_qry_record()->
    "record?;".

vsis_qry_vol_def()->
    "vol_def?VOLUME_NAME".

vsis_qry_data_check()->
    "data_check?VOLUME_NAME:PROJECT_NAME:INTERFACE_NAME;".

vsis_qry_disk_model()->
    "disk_model?VOLUME_NAME;".

vsis_qry_disk_serial()->
    "disk_serial?VOLUME_NAME;".

vsis_qry_disk_size()->
    "disk_size?VOLUME_NAME;".

vsis_qry_disk_perf()->
    "disk_perf?VOLUME_NAME;".

vsis_qry_dts_id()->
    "dts_id?".

vsis_qry_status()->
    "status?;".

vsis_qry_rtime()->
    "rtime?VOLUME_NAME:RECORDING_SPEED;".

vsis_qry_vol_usage()->
    "vol_usage?VOLUME_NAME:RECORDING_SPEED;".

						% TODO: Randomize response values.
x3c_resp_set_volume_state()->
    #x3c_resp{name="set_volume_state", retval="ok", reason="Rsn",
	      params=[#param{pname="volumeName", type="string", value="VOL1"}]}.

x3c_resp_set_volume_state_mask()->
    #x3c_resp{name="set_volume_state_mask", retval="ok", reason="Rsn",
	      params=[#param{pname="volumeName", type="string", value="VOL1"}]}.

x3c_resp_set_input_stream()->
    #x3c_resp{name="set_input_stream", retval="ok", reason="Rsn",
	      params=[]}.

x3c_resp_set_module_serial_number()->
    #x3c_resp{name="set_module_serial_number", retval="ok", reason="Rsn",
	      params=[#param{pname="msn", type="string", value="MSN1"},
		      #param{pname="volumeName", type="string", value="VOL1"}]}.

x3c_resp_set_record()->
    #x3c_resp{name="set_record", retval="ok", reason="Rsn",
	      params=[]}.

x3c_resp_define_volume()->
    #x3c_resp{name="define_volume", retval="ok", reason="Rsn",
	      params=[]}.

x3c_resp_set_volume_action()->
    #x3c_resp{name="set_volume_action", retval="ok", reason="Rsn",
	      params=[]}.

x3c_resp_get_volume_state()->
    #x3c_resp{name="get_volume_state", retval="ok", reason="Rsn",
	      params=[#param{pname="volumeName", type="string", value="VOL1"},
		      #param{pname="dsmStatus", type="keyword", value="recorded"}]}.

x3c_resp_get_volume_state_mask()->
						% Check this command return.
    #x3c_resp{name="get_volume_state_mask", retval="ok", reason="Rsn",
	      params=[#param{pname="volumeName", type="string", value="VOL1"}]}.

x3c_resp_get_input_stream()->
    #x3c_resp{name="get_input_stream", retval="ok", reason="Rsn",
	      params=[#param{pname="streamLabel", type="string", value="STRM1"},
		      #param{pname="interfaceID", type="string", value="eth0"},
		      #param{pname="dataFormat", type="keyword", value="VDIF"},
		      #param{pname="packetFilter", type="keyword", value="FULL"},
		      #param{pname="payloadOffset", type="unsigned", value="0"},
		      #param{pname="ipAddress", type="ipAddress", value="127.0.0.1"}]}.

x3c_resp_get_module_serial_number()->
    #x3c_resp{name="get_module_serial_number", retval="ok", reason="Rsn",
	      params=[#param{pname="MSN", type="string", value="MSN1"},
		      #param{pname="volumeName", type="string", value="VOL1"},
		      #param{pname="MSNCount", type="unsigned", value="MSN1"}]}.

x3c_resp_get_record_status()->
    #x3c_resp{name="get_record_status", retval="ok", reason="Rsn",
	      params=[#param{pname="scanName", type="string", value="SCAN1"},
		      #param{pname="scanNumber", type="unsigned", value="1"},
		      #param{pname="scanStatus", type="string", value="ON"}]}.

x3c_resp_get_volume_info()->
    #x3c_resp{name="get_volume_info", retval="ok", reason="Rsn",
	      params=[#param{pname="volumeName", type="string", value="VOL1"},
		      #param{pname="volumeCont", type="unsigned", value="8"},
		      #param{pname="phyVolumeName", type="string", value="PVOL1"}]}.

x3c_resp_check_data()->
    #x3c_resp{name="check_data", retval="ok", reason="Rsn",
	      params=[#param{pname="dataOk", type="boolean", value="true"},
		      #param{pname="interfaceName", type="string", value="INT1"},
		      #param{pname="dataLength", type="unsigned long", value="42"},
		      #param{pname="droppedBytes", type="unsigned long", value="43"},
		      #param{pname="startTime", type="time", value="12:00:01"},
		      #param{pname="recordingRate", type="float", value="8.0"}]}.

x3c_resp_get_disk_data()->
    #x3c_resp{name="get_disk_data", retval="ok", reason="Rsn",
	      params=[#param{pname="diskCount", type="unsigned", value="8"},
		      #param{pname="volumeName", type="string", value="VOL1"},
		      #param{pname="volumeSerial", type="string", value="VSN1"},
		      #param{pname="volumePerformance", type="unsigned", value="9"},
		      #param{pname="volumeModel", type="string", value="VMOD1"},
		      #param{pname="diskModel", type="string", value="DMOD1"},
		      #param{pname="diskSerial", type="string", value="DSER1"},
		      #param{pname="diskSize", type="unsigned", value="1000"},
		      #param{pname="diskPerformance", type="unsigned", value="9"}]}.

x3c_resp_get_disk_performance()->
    #x3c_resp{name="get_disk_performance", retval="ok", reason="Rsn",
	      params=[#param{pname="diskCount", type="unsigned", value="8"},
		      #param{pname="volumeName", type="string", value="VOL1"},
		      #param{pname="volumeSerial", type="string", value="VSN1"},
		      #param{pname="volumePerformance", type="unsigned", value="9"},
		      #param{pname="volumeModel", type="string", value="VMOD1"},
						% missing check others
		      #param{pname="diskModel", type="string", value="DMOD1"},
		      #param{pname="diskSerial", type="string", value="DSER1"},
		      #param{pname="diskPerformance", type="unsigned", value="9"},
		      #param{pname="averageResponseTime", type="float", value="9.8"},
		      #param{pname="averageDataRate", type="float", value="8.8"},
		      #param{pname="numberErrors", type="unsigned", value="42"}]}.

x3c_resp_get_recorder_info()->
    #x3c_resp{name="get_recorder_info", retval="ok", reason="Rsn",
	      params=[#param{pname="name", type="string", value="REC1"},
		      #param{pname="systemType", type="string", value="LGR_xx_xx-xx"},
		      #param{pname="serialNumber", type="string", value="SN42"},
		      #param{pname="swVersionNumber", type="string", value="V10"},
		      #param{pname="hwVersionNumber", type="string", value="V9"},
		      #param{pname="osVersion", type="string", value="V2.4"},
						% listed as time in docs.
		      #param{pname="cmdVersionLevel", type="string", value="V9"}]}.

x3c_resp_get_system_status()->
    #x3c_resp{name="get_system_status", retval="ok", reason="Rsn",
	      params=[#param{pname="systemReady", type="boolean", value="true"},
		      #param{pname="messagePending", type="boolean", value="true"},
		      #param{pname="activity", type="keyword", value="idle"},
		      #param{pname="problems", type="keyword", value="dataloss"}]}.

x3c_resp_get_volume_usage()->
    #x3c_resp{name="get_volume_usage", retval="ok", reason="Rsn",
	      params=[#param{pname="freeTime", type="unsigned", value="42"},
		      #param{pname="freeSpace", type="unsigned", value="43"},
		      #param{pname="usedSpace", type="unsigned", value="44"},
		      #param{pname="totalSpace", type="unsigned", value="120"}]}.


m6_x3c_commands() ->
    [
     {define_volume,
      {"myaction", "mynewvolumename", "myvolumename"}},
     {set_input_stream,
      {"mystreamlabel", "mydataformat", "myinterfaceid",
       "mypacketfilter", "mypayloadoffset", "myipaddress"}},
     {set_module_serial_number, {"myvolumename", "mymsn"}},
     {set_record,
      {"myaction", "myscanname", "myexperimentname",
       "mystationcode", "myfilenamesuffix"}},
     {set_volume_action,
      {"myvolumenname", "myaction"}},
     {set_volume_state,
      {"myvolumename", "mydsmstatus"}},
     {set_volume_state_mask,
      {"myvolumename", "myeraseenable", "myreplayenable",
       "myrecordenable"}},
     {check_data,
      {"myvolumename", "myprojectname", "myinterfacename"}},
     {get_disk_data,
      {"myvolumename"}},
     {get_disk_performance,
      {"myvolumename"}},
     {get_input_stream,
      {"mystreamlabel"}},
     {get_module_serial_number,
      {"myvolumename"}},
     {get_record_status, {}},
     {get_recorder_info, {}},
     {get_system_status, {}},
     {get_volume_info,
      {"myvolumename"}},
     {get_volume_state,
      {"myvolumename"}},
     {get_volume_usage,
      {"myvolumename", "myrecordingspeed"}}
    ].

m6_x3c_responses() ->
    [
     x3c_resp_define_volume(),
     x3c_resp_set_input_stream(),
     x3c_resp_set_module_serial_number(),
     x3c_resp_set_record(),
     x3c_resp_set_volume_action(),
     x3c_resp_set_volume_state(),
     x3c_resp_set_volume_state_mask(),
     x3c_resp_check_data(),
     x3c_resp_get_disk_data(),
     x3c_resp_get_disk_performance(),
     x3c_resp_get_input_stream(),
     x3c_resp_get_module_serial_number(),
     x3c_resp_get_record_status(),
     x3c_resp_get_recorder_info(),
     x3c_resp_get_system_status(),
     x3c_resp_get_volume_info(),
     x3c_resp_get_volume_state(),
     x3c_resp_get_volume_usage()
    ].
