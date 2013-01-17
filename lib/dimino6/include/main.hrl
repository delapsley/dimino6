%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Defines
%%%-------------------------------------------------------------------

%% M6 Server.
-define(DEFAULT_LISTEN_PORT, 14242).
-define(DEFAULT_BLOCK_SIZE, 1024).

%% Xcube connectivity.
-define(DEFAULT_XCUBE_HOST, "localhost").
-define(DEFAULT_XCUBE_PORT, 5033).

-define(TCP_OPTIONS,[list,{packet, 0},{active, false},{reuseaddr, true}]).

-define(XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" ?>").

%% Enable/disable eunit tests.
-define(NOTEST, false).

%%%-------------------------------------------------------------------
%% Records
%%%-------------------------------------------------------------------

%% For xmerl processing of x3c responses.
-record(param, {pname, type, value}).
-record(x3c_resp, {retval, reason, name, params}).
