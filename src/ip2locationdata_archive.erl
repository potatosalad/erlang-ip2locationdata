%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  14 Apr 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(ip2locationdata_archive).

-include_lib("kernel/include/file.hrl").

%% API
-export([start/3]).
%% Private API
-export([execute/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

start(PackageDir, Prefix, ArchiveFilename) ->
	erlang:spawn_monitor(?MODULE, execute, [self(), PackageDir, Prefix, ArchiveFilename]).

%%%===================================================================
%%% Private API functions
%%%===================================================================

%% @private
execute(Parent, PackageDir, Prefix, ArchiveFilename) ->
	Folder = fun
		(FileInArchive, GetInfo, GetBin, false) ->
			case read_archive_package_match(FileInArchive, GetInfo) of
				{true, _Name, Extension, Size} ->
					PackageFilename = to_char_list(filename:join(PackageDir, iolist_to_binary([Prefix, Extension]))),
					case file_size_check(PackageFilename, Size) of
						true ->
							PackageFilename;
						false ->
							try GetBin() of
								Contents when is_binary(Contents) andalso byte_size(Contents) == Size ->
									ok = file:write_file(PackageFilename, Contents, [raw]),
									PackageFilename
							catch
								_:_ ->
									false
							end
					end;
				false ->
					false
			end;
		(_FileInArchive, _GetInfo, _GetBin, AccIn) ->
			AccIn
	end,
	try zip:foldl(Folder, false, ArchiveFilename) of
		{ok, false} ->
			Parent ! {self(), false},
			exit(normal);
		{ok, PackageFilename} ->
			Parent ! {self(), {true, PackageFilename}},
			exit(normal);
		_ ->
			Parent ! {self(), false},
			exit(normal)
	catch
		_:_ ->
			Parent ! {self(), false},
			exit(normal)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

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
read_archive_package_match(Name, GetInfo) ->
	case iolist_to_binary(string:to_lower(filename:extension(Name))) of
		Extension when Extension == <<".bin">> orelse Extension == <<".csv">> ->
			try GetInfo() of
				#file_info{size=Size} ->
					{true, Name, Extension, Size};
				_ ->
					false
			catch
				_:_ ->
					false
			end;
		_ ->
			false
	end.

%% @private
to_char_list(List) when is_list(List) ->
	List;
to_char_list(Binary) when is_binary(Binary) ->
	binary:bin_to_list(Binary).
