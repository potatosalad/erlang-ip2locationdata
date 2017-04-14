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
-module(ip2locationdata).

%% API
-export([autoupdate/0]).
-export([autoupdate/1]).
-export([credentials/0]).
-export([credentials/1]).
-export([data_dir/0]).
-export([data_dir/1]).
-export([endpoints/0]).
-export([endpoints/1]).
-export([keep_releases/0]).
-export([keep_releases/1]).
-export([logger/0]).
-export([logger/1]).
%% Package API
-export([package_add/1]).
-export([package_get/1]).
-export([package_list/0]).
-export([package_remove/1]).
%% Private API
-export([start/0]).

%% Macros
-define(LOAD_APP(F), begin
	_ = application:load(ip2locationdata),
	F
end).
-define(MAYBE_START_APP(F), try
	F
catch
	_:_ ->
		_ = ip2locationdata:start(),
		F
end).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec autoupdate() -> boolean() | pos_integer().
autoupdate() ->
	?LOAD_APP(application:get_env(?MODULE, autoupdate, true)).

-spec autoupdate(BooleanOrSeconds::boolean() | pos_integer()) -> ok.
autoupdate(BooleanOrSeconds)
		when is_boolean(BooleanOrSeconds)
		orelse (is_integer(BooleanOrSeconds) andalso BooleanOrSeconds > 0) ->
	?LOAD_APP(application:set_env(?MODULE, autoupdate, BooleanOrSeconds)),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

credentials() ->
	?LOAD_APP(application:get_env(?MODULE, credentials, undefined)).

credentials(Credentials = #{ login := _, password := _ }) ->
	?LOAD_APP(application:set_env(?MODULE, credentials, Credentials)),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

data_dir() ->
	case ?LOAD_APP(application:get_env(?MODULE, data_dir)) of
		{ok, Directory} when is_binary(Directory) orelse is_list(Directory) ->
			Directory;
		_ ->
			priv_dir()
	end.

data_dir(Directory) when is_binary(Directory) orelse is_list(Directory) ->
	?LOAD_APP(application:set_env(?MODULE, data_dir, Directory)),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

endpoints() ->
	?LOAD_APP(application:get_env(?MODULE, endpoints, undefined)).

endpoints(Endpoints = #{ download := _, info := _ }) ->
	?LOAD_APP(application:set_env(?MODULE, endpoints, Endpoints)),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

keep_releases() ->
	?LOAD_APP(application:get_env(?MODULE, keep_releases, 2)).

keep_releases(Number) when is_integer(Number) andalso Number > 0 ->
	?LOAD_APP(application:set_env(?MODULE, keep_releases, Number)),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

logger() ->
	?LOAD_APP(application:get_env(?MODULE, logger, false)).

logger(Boolean) when is_boolean(Boolean) ->
	?LOAD_APP(application:set_env(?MODULE, logger, Boolean)),
	_ = ip2locationdata:start(),
	case Boolean of
		true ->
			Started = supervisor:start_child(ip2locationdata_sup, {
				ip2locationdata_logger,
				{ip2locationdata_logger, start_link, []},
				permanent, 5000, worker, [ip2locationdata_logger]
			}),
			ok = case Started of
				{ok, _} ->
					ok;
				{error, already_present} ->
					ok;
				{error, {already_started, _}} ->
					ok
			end;
		false ->
			case supervisor:terminate_child(ip2locationdata_sup, ip2locationdata_logger) of
				ok ->
					_ = supervisor:delete_child(ip2locationdata_sup, ip2locationdata_logger),
					ok;
				{error, _Reason} ->
					ok
			end
	end,
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

%%====================================================================
%% Package API functions
%%====================================================================

package_add(Package = #{ id := ID, productcode := ProductCode })
		when is_atom(ID)
		andalso (is_binary(ProductCode) orelse is_list(ProductCode)) ->
	OldPackages = package_list(),
	{IsExisting, NewPackages} = package_put(ID, OldPackages, Package, []),
	application:set_env(?MODULE, packages, NewPackages),
	_ = ip2locationdata:start(),
	case IsExisting of
		false ->
			ok;
		true ->
			ip2locationdata_package_sup:stop_package(ID)
	end,
	ip2locationdata_package_sup:start_package(ID, Package).

package_get(ID) when is_atom(ID) ->
	package_get(ID, 5000).

package_get(ID, Timeout) when is_atom(ID) ->
	?MAYBE_START_APP(ip2locationdata_package:get_package(ID, Timeout)).

package_list() ->
	?LOAD_APP(application:get_env(?MODULE, packages, [])).

package_remove(ID) when is_atom(ID) ->
	ip2locationdata_package_sup:stop_package(ID),
	Packages = package_delete(ID, package_list(), []),
	application:set_env(?MODULE, packages, Packages),
	?MAYBE_START_APP(ip2locationdata_server:config_change()).

%%====================================================================
%% Private API functions
%%====================================================================

start() ->
	case application:ensure_all_started(?MODULE) of
		{ok, _} ->
			ok;
		StartError ->
			StartError
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
package_delete(ID, [#{ id := ID } | T], Acc) ->
	lists:flatten([Acc | T]);
package_delete(ID, [H | T], Acc) ->
	package_delete(ID, T, [Acc, H]);
package_delete(_ID, [], Acc) ->
	lists:flatten(Acc).

%% @private
package_put(ID, [#{ id := ID } | T], Package, Acc) ->
	{true, lists:flatten([Acc, Package | T])};
package_put(ID, [H | T], Package, Acc) ->
	package_put(ID, T, Package, [Acc, H]);
package_put(_ID, [], Package, Acc) ->
	{false, lists:flatten([Acc, Package])}.

%% @private
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

% %% @private
% to_char_list(List) when is_list(List) ->
% 	List;
% to_char_list(Binary) when is_binary(Binary) ->
% 	binary:bin_to_list(Binary).
