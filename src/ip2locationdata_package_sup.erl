%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  13 Apr 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(ip2locationdata_package_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_package/2]).
-export([stop_package/1]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(SUPERVISOR, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_package(ID, Package = #{ id := ID, productcode := _ }) ->
	supervisor:start_child(?SUPERVISOR, ip2locationdata_package:child_spec(ID, Package)).

stop_package(ID) ->
	case supervisor:terminate_child(?SUPERVISOR, {ip2locationdata_package, ID}) of
		ok ->
			_ = supervisor:delete_child(?SUPERVISOR, {ip2locationdata_package, ID}),
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	ChildSpecs = child_specs(),
	Restart = {one_for_one, 1, 1},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
child_specs() ->
	[begin
		ip2locationdata_package:child_spec(ID, Package)
	end || Package = #{ id := ID } <- ip2locationdata:package_list()].
