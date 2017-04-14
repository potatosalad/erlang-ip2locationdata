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
-module(ip2locationdata_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	ip2locationdata = ets:new(ip2locationdata, [
		named_table,
		ordered_set,
		public,
		{read_concurrency, true}
	]),
	LoggerChildSpec = case ip2locationdata:logger() of
		true ->
			?CHILD(ip2locationdata_logger, worker);
		false ->
			[]
	end,
	ChildSpecs = lists:flatten([
		{ip2locationdata_event:manager(),
			{gen_event, start_link, [{local, ip2locationdata_event:manager()}]},
			permanent, 5000, worker, [gen_event]},
		LoggerChildSpec,
		?CHILD(ip2locationdata_server, worker),
		?CHILD(ip2locationdata_package_sup, supervisor)
	]),
	Restart = {one_for_one, 1, 1},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
