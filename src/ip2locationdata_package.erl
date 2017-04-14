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
-module(ip2locationdata_package).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/2]).
-export([child_spec/2]).
-export([get_package/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-type package() :: #{
	id := atom(),
	productcode := iodata(),
	credentials => #{
		login := iodata(),
		password := iodata()
	},
	endpoints => #{
		download := iodata(),
		info := iodata()
	}
}.

-export_type([package/0]).

-record(metadata, {
	id        = undefined :: undefined | atom(),
	expire_at = undefined :: undefined | binary(),
	size      = undefined :: undefined | non_neg_integer(),
	version   = undefined :: undefined | binary()
}).

-record(release, {
	prefix   = undefined :: undefined | binary(),
	metadata = undefined :: undefined | #metadata{},
	archive  = undefined :: undefined | file:filename_all(),
	sidecar  = undefined :: undefined | file:filename_all(),
	package  = undefined :: undefined | file:filename_all()
}).

-record(state, {
	id              = undefined   :: undefined | atom(),
	package         = undefined   :: undefined | package(),
	name            = idle        :: idle | poll | wait,
	last_checked    = 0           :: integer(),
	timer_reference = undefined   :: undefined | reference(),
	queue           = queue:new() :: queue:queue({term(), {pid(), reference()}}),
	releases        = []          :: [#release{}],
	release         = undefined   :: undefined | #release{},
	request         = undefined   :: undefined | {reference(), #release{}}
}).

%% Macros
-define(RETRY_AFTER, 10000). % timer:seconds(10).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(ID, Package = #{ id := ID, productcode := _ }) ->
	gen_server:start_link({via, ip2locationdata_server, ID}, ?MODULE, {ID, Package}, []).

child_spec(ID, Package = #{ id := ID, productcode := _ }) ->
	{{ip2locationdata_package, ID},
		{ip2locationdata_package, start_link, [ID, Package]},
		transient, 5000, worker, [ip2locationdata_package]}.

get_package(ID, Timeout) ->
	gen_server:call({via, ip2locationdata_server, ID}, get_package, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({ID, Package = #{ id := ID, productcode := _ }}) ->
	State = #state{id=ID, package=Package},
	{ok, start_timer(State, 0)}.

%% @private
handle_call(Request=get_package, From, State=#state{queue=Queue, release=undefined}) ->
	{noreply, State#state{queue=queue:in({Request, From}, Queue)}};
handle_call(get_package, _From, State=#state{release=#release{package=PackageFilename}}) ->
	{reply, {ok, PackageFilename}, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(config_change, State=#state{name=idle}) ->
	{noreply, start_timer(State, 0)};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({timeout, TimerReference, poll}, State=#state{name=idle, timer_reference=TimerReference}) ->
	maybe_poll(State#state{timer_reference=undefined}, os:system_time(seconds));
handle_info({http, {RequestId, saved_to_file}}, State=#state{name=wait, id=ID, request={RequestId, Release=#release{metadata=#metadata{size=Size}, archive=ArchiveFilename}}}) ->
	case file_size_check(ArchiveFilename, Size) of
		true ->
			ok = write_release(Release),
			DataDir = ip2locationdata:data_dir(),
			PackageDir = get_package_directory(State, DataDir),
			case read_archive_file(PackageDir, Release) of
				{true, NewRelease=#release{package=PackageFilename}} ->
					ok = ip2locationdata_event:announce(ID, PackageFilename),
					{noreply, maybe_cleanup(State#state{release=NewRelease, request=undefined})};
				false ->
					ok = delete_release(Release),
					{noreply, start_timer(State, ?RETRY_AFTER)}
			end;
		false ->
			ok = delete_release(Release),
			{noreply, start_timer(State, ?RETRY_AFTER)}
	end;
handle_info({http, {_RequestId, _}}, State=#state{name=wait}) ->
	{noreply, start_timer(State, ?RETRY_AFTER)};
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

%% @private
cleanup(State=#state{queue=Queue}, Releases, Release=#release{prefix=Prefix}) ->
	KeepReleases = case ip2locationdata:keep_releases() of
		Integer when is_integer(Integer) andalso Integer >= 1 ->
			Integer;
		_ ->
			1
	end,
	OldReleases = droplast([R || R=#release{prefix=P} <- Releases, P =/= Prefix] ++ [Release], KeepReleases),
	_ = [delete_release(R) || R <- OldReleases],
	flush_queue(Queue, State#state{releases=[], queue=queue:new()}).

%% @private
delete_release(#release{archive=AF, sidecar=SF, package=PF}) ->
	ok = maybe_delete_file(AF),
	ok = maybe_delete_file(SF),
	ok = maybe_delete_file(PF),
	ok.

%% @private
droplast([], _) ->
	[];
droplast(List, 0) ->
	List;
droplast(List, N) ->
	droplast(lists:droplast(List), N - 1).

%% @private
flush_queue(Queue0, State0) ->
	case queue:out(Queue0) of
		{{value, {Request, From}}, Queue1} ->
			case handle_call(Request, From, State0) of
				{noreply, State1} ->
					flush_queue(Queue1, State1);
				{reply, Reply, State1} ->
					_ = gen_server:reply(From, Reply),
					flush_queue(Queue1, State1)
			end;
		{empty, Queue0} ->
			State0
	end.

%% @private
get_autoupdate_timeout() ->
	case ip2locationdata:autoupdate() of
		true ->
			timer:hours(24) div timer:seconds(1);
		Integer when is_integer(Integer) andalso Integer > 0 ->
			Integer;
		_ ->
			false
	end.

%% @private
get_package_directory(#state{id=ID}, DataDir) ->
	ReleasesDir = get_releases_directory(DataDir),
	PackageDir = filename:join(ReleasesDir, atom_to_list(ID)),
	ok = filelib:ensure_dir(filename:join(PackageDir, ".keep")),
	PackageDir.

%% @private
get_next_release_prefix() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
	lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Year, Month, Day, Hour, Minute, Second])).

%% @private
get_releases_directory(DataDir) ->
	ReleasesDir = filename:join(DataDir, "releases"),
	ok = filelib:ensure_dir(filename:join(ReleasesDir, ".keep")),
	ReleasesDir.

%% @private
is_release_outdated(State=#state{id=ID, release=undefined}, Releases) when length(Releases) > 0 ->
	Release=#release{package=Filename} = lists:last(Releases),
	ok = ip2locationdata_event:announce(ID, Filename),
	is_release_outdated(State#state{release=Release}, Releases);
is_release_outdated(State=#state{id=ID, release=OldRelease}, _Releases) ->
	#{
		productcode := ProductCode,
		credentials := #{ login := Login, password := Password },
		endpoints := #{ info := TemplateURL }
	} = package_resolve(State),
	InfoURL = binary:bin_to_list(iolist_to_binary([
		TemplateURL,
		$?, "email=", http_uri:encode(to_char_list(Login)),
		$&, "password=", http_uri:encode(to_char_list(Password)),
		$&, "productcode=", http_uri:encode(to_char_list(ProductCode))
	])),
	case httpc:request(get, {InfoURL, []}, [], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, Body}} ->
			case binary:split(Body, <<";">>, [global, trim_all]) of
				[<<"OK">>, ExpireAt, Version, RawSize, _] ->
					case is_release_version(OldRelease, Version) of
						true ->
							ok = ip2locationdata_event:announce(ID, OldRelease#release.package),
							{false, State};
						false ->
							Size = binary_to_integer(RawSize),
							Metadata = #metadata{
								id = ID,
								expire_at = ExpireAt,
								size = Size,
								version = Version
							},
							{true, Metadata, State}
					end;
				_ ->
					{true, State}
			end;
		_ ->
			{true, State}
	end.

%% @private
is_release_version(#release{metadata=#metadata{version=Version}}, Version) ->
	true;
is_release_version(_, _) ->
	false.

%% @private
maybe_cleanup(State=#state{releases=Releases, release=Release, request=undefined}) when Release =/= undefined ->
	case get_autoupdate_timeout() of
		false ->
			start_timer(cleanup(State, Releases, Release), timer:hours(1));
		Autoupdate ->
			start_timer(cleanup(State, Releases, Release), timer:seconds(Autoupdate))
	end;
maybe_cleanup(State) ->
	State.

%% @private
maybe_delete_file(undefined) ->
	ok;
maybe_delete_file(Filename) ->
	_ = file:delete(Filename),
	ok.

%% @private
maybe_poll(State=#state{last_checked=LastChecked}, Now) ->
	case get_autoupdate_timeout() of
		false ->
			{noreply, start_timer(State, timer:hours(1))};
		Autoupdate ->
			case (Now - LastChecked) >= Autoupdate of
				false ->
					{noreply, start_timer(State, timer:seconds((LastChecked + Autoupdate) - Now))};
				true ->
					poll(State#state{name=poll})
			end
	end.

%% @private
package_resolve(#state{package=PackageTemplate}) ->
	PackageDefault = #{
		credentials => application:get_env(ip2locationdata, credentials, #{ login => <<>>, password => <<>> }),
		endpoints => application:get_env(ip2locationdata, endpoints, #{ download => <<>>, info => <<>> })
	},
	maps:merge(PackageDefault, PackageTemplate).

%% @private
poll(State0=#state{id=ID}) ->
	case [X || #{ id := X } <- ip2locationdata:package_list(), X == ID] of
		[ID] ->
			ok = ip2locationdata_event:refresh(ID),
			DataDir = ip2locationdata:data_dir(),
			{State1, Releases} = read_sidecar_files(State0, DataDir),
			case is_release_outdated(State1, Releases) of
				{true, State2} ->
					{noreply, start_timer(State2, ?RETRY_AFTER)};
				{true, Metadata, State2} ->
					request_release(State2, DataDir, Metadata);
				{false, State2} ->
					{noreply, maybe_cleanup(State2)}
			end;
		_ ->
			{stop, normal, State0}
	end.

%% @private
file_size_check(Filename, Size) ->
	try file:read_file_info(Filename, [raw]) of
		{ok, #file_info{size=Size}} ->
			true;
		_ ->
			false
	catch
		_:_ ->
			false
	end.

%% @private
read_archive_file(PackageDir, Release=#release{metadata=#metadata{size=ArchiveSize}, archive=ArchiveFilename}) ->
	try file:read_file_info(ArchiveFilename, [raw]) of
		{ok, #file_info{size=ArchiveSize}} ->
			read_archive_package(PackageDir, Release);
		_ ->
			false
	catch
		_:_ ->
			false
	end.

%% @private
read_archive_package(PackageDir, Release=#release{prefix=Prefix, archive=ArchiveFilename}) ->
	{Pid, MonitorRef} = ip2locationdata_archive:start(PackageDir, Prefix, ArchiveFilename),
	Res0 = receive
		{Pid, Result} ->
			ok = receive
				{'DOWN', MonitorRef, process, Pid, _Info} ->
					ok
			end,
			{ok, Result};
		{'DOWN', MonitorRef, process, Pid, _Info} ->
			error
	end,
	case Res0 of
		{ok, {true, PackageFilename}} ->
			NewRelease = Release#release{
				package = PackageFilename
			},
			{true, NewRelease};
		_ ->
			false
	end.

%% @private
read_sidecar_file(#state{id=ID}, PackageDir, Release=#release{prefix=Prefix, sidecar=SidecarFilename}) ->
	try file:read_file(SidecarFilename) of
		{ok, Contents} ->
			try erlang:binary_to_term(Contents, [safe]) of
				Metadata=#metadata{id=ID} ->
					ArchiveFilename = to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, ".zip"]))),
					NewRelease = Release#release{
						metadata = Metadata,
						archive = ArchiveFilename
					},
					read_archive_file(PackageDir, NewRelease);
				_ ->
					false
			catch
				_:_ ->
					false
			end;
		_ ->
			false
	catch
		_:_ ->
			false
	end.

%% @private
read_sidecar_files(State, DataDir) ->
	PackageDir = get_package_directory(State, DataDir),
	Folder = fun(Filename, Acc) ->
		case iolist_to_binary(filename:extension(Filename)) of
			<<".etf">> ->
				Prefix = iolist_to_binary(filename:basename(Filename, filename:extension(Filename))),
				Release = #release{
					prefix = Prefix,
					sidecar = Filename
				},
				case read_sidecar_file(State, PackageDir, Release) of
					{true, NewRelease} ->
						ordsets:add_element(NewRelease, Acc);
					false ->
						Acc
				end;
			_ ->
				Acc
		end
	end,
	Releases = filelib:fold_files(PackageDir, [], false, Folder, ordsets:new()),
	{State#state{releases=Releases}, Releases}.

%% @private
request_release(State, DataDir, Metadata) ->	
	#{
		productcode := ProductCode,
		credentials := #{ login := Login, password := Password },
		endpoints := #{ download := TemplateURL }
	} = package_resolve(State),
	DownloadURL = binary:bin_to_list(iolist_to_binary([
		TemplateURL,
		$?, "login=", http_uri:encode(to_char_list(Login)),
		$&, "password=", http_uri:encode(to_char_list(Password)),
		$&, "productcode=", http_uri:encode(to_char_list(ProductCode))
	])),
	PackageDir = get_package_directory(State, DataDir),
	Prefix = iolist_to_binary(get_next_release_prefix()),
	ArchiveFilename = to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, ".zip"]))),
	SidecarFilename = to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, ".etf"]))),
	_ = file:delete(ArchiveFilename),
	_ = file:delete(SidecarFilename),
	_ = file:delete(to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, ".bin"])))),
	_ = file:delete(to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, ".csv"])))),
	Release = #release{
		prefix = Prefix,
		metadata = Metadata,
		archive = ArchiveFilename,
		sidecar = SidecarFilename
	},
	case httpc:request(get, {DownloadURL, []}, [], [{receiver, self()}, {stream, ArchiveFilename}, {sync, false}]) of
		{ok, RequestId} ->
			{noreply, State#state{name=wait, request={RequestId, Release}}};
		_ ->
			{noreply, start_timer(State, ?RETRY_AFTER)}
	end.

%% @private
start_timer(State=#state{timer_reference=undefined, request=undefined}, Time) ->
	TimerReference = erlang:start_timer(Time, self(), poll),
	State#state{name=idle, timer_reference=TimerReference};
start_timer(State=#state{timer_reference=undefined, request={RequestId, _, Filename}}, Time) ->
	ok = httpc:cancel_request(RequestId),
	_ = file:delete(Filename),
	start_timer(State#state{request=undefined}, Time);
start_timer(State=#state{timer_reference=TimerReference}, Time) ->
	catch erlang:cancel_timer(TimerReference),
	start_timer(State#state{timer_reference=undefined}, Time).

%% @private
to_char_list(List) when is_list(List) ->
	List;
to_char_list(Binary) when is_binary(Binary) ->
	binary:bin_to_list(Binary).

%% @private
write_release(#release{metadata=Metadata, sidecar=SidecarFilename}) ->
	Contents = erlang:term_to_binary(Metadata),
	ok = file:write_file(SidecarFilename, Contents).
