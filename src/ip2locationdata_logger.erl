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
-module(ip2locationdata_logger).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	ok = ip2locationdata_event:add_handler(ip2locationdata_event_handler, self()),
	{ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'$ip2locationdata-event', Event}, State) ->
	ok = error_logger:info_msg("ip2locationdata event: ~p~n", [Event]),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok = ip2locationdata_event:delete_handler(ip2locationdata_event_handler, self()),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	ok = ip2locationdata_event:delete_handler(ip2locationdata_event_handler, self()),
	ok = ip2locationdata_event:add_handler(ip2locationdata_event_handler, self()),
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
