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
-module(ip2locationdata_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([config_change/0]).

%% Name Server API
-export([register_name/2]).
-export([send/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%% Macros
-define(SERVER, ?MODULE).
-define(TAB, ip2locationdata).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

config_change() ->
	gen_server:call(?SERVER, config_change).

%%%===================================================================
%%% Name Server API functions
%%%===================================================================

-spec register_name(Key::atom(), Pid::pid()) -> yes | no.
register_name(Key, Pid) when is_atom(Key) andalso is_pid(Pid) ->
	gen_server:call(?SERVER, {register_name, Key, Pid}, infinity).

-spec send(Key::atom(), Msg::term()) -> pid().
send(Key, Msg) when is_atom(Key) ->
	case whereis_name(Key) of
		Pid when is_pid(Pid) ->
			Pid ! Msg,
			Pid;
		undefined ->
			erlang:error(badarg, [Key, Msg])
	end.

-spec unregister_name(Key::atom()) -> ok.
unregister_name(Key) when is_atom(Key) ->
	case whereis_name(Key) of
		undefined ->
			ok;
		Pid ->
			ip2locationdata_sup:stop_package(Pid),
			ok
	end.

-spec whereis_name(Key::atom()) -> pid() | undefined.
whereis_name(Key) when is_atom(Key) ->
	case ets:lookup(?TAB, Key) of
		[{Key, Pid}] ->
			case erlang:is_process_alive(Pid) of
				true ->
					Pid;
				false ->
					undefined
			end;
		[] ->
			undefined
	end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} || [Ref, Pid] <- ets:match(?TAB, {'$1', '$2'})],
	{ok, #state{monitors=Monitors}}.

%% @private
handle_call(config_change, _From, State=#state{monitors=Monitors}) ->
	_ = [begin
		gen_server:cast(Pid, config_change)
	end || {{_, Pid}, _} <- Monitors],
	{reply, ok, State};
handle_call({register_name, Key, Pid}, _From, State=#state{monitors=Monitors}) ->
	case ets:insert_new(?TAB, {Key, Pid}) of
		true ->
			MonitorRef = erlang:monitor(process, Pid),
			{reply, yes, State#state{monitors=[{{MonitorRef, Pid}, Key} | Monitors]}};
		false ->
			{reply, no, State}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State=#state{monitors=Monitors}) ->
	case lists:keytake({MonitorRef, Pid}, 1, Monitors) of
		{value, {_, Ref}, NewMonitors} ->
			true = ets:delete(?TAB, Ref),
			{noreply, State#state{monitors=NewMonitors}};
		false ->
			{noreply, State}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
