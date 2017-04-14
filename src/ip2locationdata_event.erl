%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  12 Apr 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(ip2locationdata_event).

%% API
-export([manager/0]).
-export([add_handler/2]).
-export([delete_handler/2]).
-export([refresh/1]).
-export([announce/2]).

%% Macros
-define(MANAGER, ip2locationdata_manager).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(?MANAGER, Handler, Pid).

delete_handler(Handler, Pid) ->
	gen_event:delete_handler(?MANAGER, Handler, Pid).

refresh(ID) ->
	notify({package, refresh, ID}).

announce(ID, Filename) ->
	notify({package, announce, ID, Filename}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(?MANAGER, Message).
