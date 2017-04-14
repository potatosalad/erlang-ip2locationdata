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
-module(ip2locationdata_adapter_bin).

%% API
-export([read/1]).
-export([query/2]).

%% Records
-record(ip2locationdata_adapter_bin, {
	file        = undefined :: undefined | file:filename_all(),
	type        = undefined :: undefined | non_neg_integer(),
	column      = undefined :: undefined | non_neg_integer(),
	year        = undefined :: undefined | non_neg_integer(),
	month       = undefined :: undefined | non_neg_integer(),
	day         = undefined :: undefined | non_neg_integer(),
	ipv4_count  = undefined :: undefined | non_neg_integer(),
	ipv4_addr   = undefined :: undefined | non_neg_integer(),
	ipv6_count  = undefined :: undefined | non_neg_integer(),
	ipv6_addr   = undefined :: undefined | non_neg_integer(),
	ipv4_index  = undefined :: undefined | non_neg_integer(),
	ipv6_index  = undefined :: undefined | non_neg_integer(),
	ipv4_column = undefined :: undefined | non_neg_integer(),
	ipv6_column = undefined :: undefined | non_neg_integer()
}).

%% Macros
-define(DEFAULT_BINARY, <<"This parameter is unavailable for selected data file. Please upgrade the data file.">>).

%%%===================================================================
%%% API functions
%%%===================================================================

read(Filename) ->
	open(Filename, fun read_adapter/2, [Filename]).

query(Adapter=#ip2locationdata_adapter_bin{file=Filename}, Ip) ->
	open(Filename, fun query/3, [Adapter, Ip]).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
fetch_column_position(Column, Index) ->
	try element(Index, Column) of
		Position ->
			{ok, Position}
	catch
		_:_ ->
			error
	end.

%% @private
open(Filename, Function, Arguments) ->
	try file:open(Filename, [read, binary, raw]) of
		{ok, FileHandle} ->
			try
				erlang:apply(Function, [FileHandle | Arguments])
			catch
				Class:Exception ->
					{error, {Class, Exception, erlang:get_stacktrace()}}
			after
				_ = file:close(FileHandle)
			end;
		OpenError ->
			OpenError
	catch
		Class:Exception ->
			{error, {Class, Exception}}
	end.

%% @private
query(H, Adapter=#ip2locationdata_adapter_bin{}, Ip) ->
	case inet:parse_address(to_ipaddr(Ip)) of
		{ok, {X1, X2, X3, X4}} ->
			Ipnum = (X1 bsl 24) + (X2 bsl 16) + (X3 bsl 8) + (X4),
			query_ipv4(H, Adapter, Ipnum);
		{ok, {X1, X2, X3, X4, X5, X6, X7, X8}} ->
			Ipnum = (X1 bsl 112) + (X2 bsl 96) + (X3 bsl 80) + (X4 bsl 64) + (X5 bsl 48) + (X6 bsl 32) + (X7 bsl 16) + X8,
			case Ipnum >= 281470681743360 andalso Ipnum =< 281474976710655 of
				true ->
					query_ipv4(H, Adapter, Ipnum - 281470681743360);
				false ->
					query_ipv6(H, Adapter, Ipnum)
			end;
		ParseAddressError ->
			ParseAddressError
	end.

%% @private
query_ipv4(H, #ip2locationdata_adapter_bin{type=Dbtype, ipv4_addr=Ipv4Addr, ipv4_index=Ipv4Index, ipv4_column=Ipv4Column}, Ipnum) when Ipv4Index > 0 ->
	Indexpos = ((Ipnum bsr 16) bsl 3) + Ipv4Index,
	Low = read_uint32(H, Indexpos),
	High = read_uint32(H, Indexpos + 4),
	searchtree(H, Ipnum, Dbtype, Low, High, Ipv4Addr, Ipv4Column, ipv4);
query_ipv4(H, #ip2locationdata_adapter_bin{type=Dbtype, ipv4_count=High, ipv4_addr=Ipv4Addr, ipv4_column=Ipv4Column}, Ipnum) ->
	searchtree(H, Ipnum, Dbtype, 0, High, Ipv4Addr, Ipv4Column, ipv4).

%% @private
query_ipv6(H, #ip2locationdata_adapter_bin{type=Dbtype, ipv6_addr=Ipv6Addr, ipv6_index=Ipv6Index, ipv6_column=Ipv6Column}, Ipnum) when Ipv6Index > 0 ->
	Indexpos = ((Ipnum bsr 112) bsl 3) + Ipv6Index,
	Low = read_uint32(H, Indexpos),
	High = read_uint32(H, Indexpos + 4),
	searchtree(H, Ipnum, Dbtype, Low, High, Ipv6Addr, Ipv6Column, ipv6);
query_ipv6(H, #ip2locationdata_adapter_bin{type=Dbtype, ipv6_count=High, ipv6_addr=Ipv6Addr, ipv6_column=Ipv6Column}, Ipnum) ->
	searchtree(H, Ipnum, Dbtype, 0, High, Ipv6Addr, Ipv6Column, ipv6).

%% @private
read_adapter(H, Filename) ->
	Column = read_uint8(H, 2),
	Adapter = #ip2locationdata_adapter_bin{
		file        = Filename,
		type        = read_uint8(H, 1),
		column      = Column,
		year        = read_uint8(H, 3),
		month       = read_uint8(H, 4),
		day         = read_uint8(H, 5),
		ipv4_count  = read_uint32(H, 6),
		ipv4_addr   = read_uint32(H, 10),
		ipv6_count  = read_uint32(H, 14),
		ipv6_addr   = read_uint32(H, 18),
		ipv4_index  = read_uint32(H, 22),
		ipv6_index  = read_uint32(H, 26),
		ipv4_column = (Column bsl 2), % 4 bytes each column
		ipv6_column = (16 + ((Column - 1) bsl 2)) % 4 bytes each column, except IPFrom column which is 16 bytes
	},
	{ok, Adapter}.

%% @private
read_column_country(H, Dbtype, Rowoffset, Column) ->
	case fetch_column_position(Column, Dbtype) of
		{ok, 0} ->
			{?DEFAULT_BINARY, ?DEFAULT_BINARY};
		{ok, Colpos} ->
			Coloffset = (Colpos - 1) bsl 2,
			X0 = read_uint32(H, Rowoffset + Coloffset),
			X1 = read_string(H, X0),
			X2 = read_string(H, X0 + 3),
			{X1, X2};
		error ->
			{error, error}
	end.

%% @private
read_column_float(H, Dbtype, Rowoffset, Column) ->
	case fetch_column_position(Column, Dbtype) of
		{ok, 0} ->
			0.0;
		{ok, Colpos} ->
			Coloffset = (Colpos - 1) bsl 2,
			X0 = read_float(H, Rowoffset + Coloffset),
			round(X0, 6);
		error ->
			error
	end.

%% @private
read_column_number(H, Dbtype, Rowoffset, Column) ->
	case fetch_column_position(Column, Dbtype) of
		{ok, 0} ->
			0.0;
		{ok, Colpos} ->
			Coloffset = (Colpos - 1) bsl 2,
			X0 = read_uint32(H, Rowoffset + Coloffset),
			X1 = read_string(H, X0),
			try
				binary_to_float(X1)
			catch
				_:_ ->
					try
						binary_to_integer(X1)
					catch
						_:_ ->
							badnum
					end
			end;
		error ->
			error
	end.

%% @private
read_column_string(H, Dbtype, Rowoffset, Column) ->
	case fetch_column_position(Column, Dbtype) of
		{ok, 0} ->
			?DEFAULT_BINARY;
		{ok, Colpos} ->
			Coloffset = (Colpos - 1) bsl 2,
			X0 = read_uint32(H, Rowoffset + Coloffset),
			X1 = read_string(H, X0),
			X1;
		error ->
			error
	end.

%% @private
read_float(H, StartPos) ->
	case file:pread(H, StartPos - 1, 4) of
		eof ->
			erlang:error({eof, [StartPos - 1, 4]});
		{ok, Data} ->
			<< F:32/float-little >> = Data,
			F
	end.

%% @private
read_record(H, Dbtype, Rowoffset) ->
	Country_position = {0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2},
	Region_position = {0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3},
	City_position = {0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4},
	Isp_position = {0, 0, 3, 0, 5, 0, 7, 5, 7, 0, 8, 0, 9, 0, 9, 0, 9, 0, 9, 7, 9, 0, 9, 7, 9},
	Latitude_position = {0, 0, 0, 0, 0, 5, 5, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
	Longitude_position = {0, 0, 0, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6},
	Domain_position = {0, 0, 0, 0, 0, 0, 0, 6, 8, 0, 9, 0, 10,0, 10, 0, 10, 0, 10, 8, 10, 0, 10, 8, 10},
	Zipcode_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 7, 0, 7, 7, 7, 0, 7, 0, 7, 7, 7, 0, 7},
	Timezone_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 7, 8, 8, 8, 7, 8, 0, 8, 8, 8, 0, 8},
	Netspeed_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 11,0, 11,8, 11, 0, 11, 0, 11, 0, 11},
	Iddcode_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 12, 0, 12, 0, 12, 9, 12, 0, 12},
	Areacode_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10 ,13 ,0, 13, 0, 13, 10, 13, 0, 13},
	Weatherstationcode_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 14, 0, 14, 0, 14, 0, 14},
	Weatherstationname_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 15, 0, 15, 0, 15, 0, 15},
	Mcc_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 16, 0, 16, 9, 16},
	Mnc_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,17, 0, 17, 10, 17},
	Mobilebrand_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,18, 0, 18, 11, 18},
	Elevation_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 19, 0, 19},
	Usagetype_position = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 20},
	{Country_short, Country_long} = read_column_country(H, Dbtype, Rowoffset, Country_position),
	Region = read_column_string(H, Dbtype, Rowoffset, Region_position),
	City = read_column_string(H, Dbtype, Rowoffset, City_position),
	Isp = read_column_string(H, Dbtype, Rowoffset, Isp_position),
	Latitude = read_column_float(H, Dbtype, Rowoffset, Latitude_position),
	Longitude = read_column_float(H, Dbtype, Rowoffset, Longitude_position),
	Domain = read_column_string(H, Dbtype, Rowoffset, Domain_position),
	Zipcode = read_column_string(H, Dbtype, Rowoffset, Zipcode_position),
	Timezone = read_column_string(H, Dbtype, Rowoffset, Timezone_position),
	Netspeed = read_column_string(H, Dbtype, Rowoffset, Netspeed_position),
	Iddcode = read_column_string(H, Dbtype, Rowoffset, Iddcode_position),
	Areacode = read_column_string(H, Dbtype, Rowoffset, Areacode_position),
	Weatherstationcode = read_column_string(H, Dbtype, Rowoffset, Weatherstationcode_position),
	Weatherstationname = read_column_string(H, Dbtype, Rowoffset, Weatherstationname_position),
	Mcc = read_column_string(H, Dbtype, Rowoffset, Mcc_position),
	Mnc = read_column_string(H, Dbtype, Rowoffset, Mnc_position),
	Mobilebrand = read_column_string(H, Dbtype, Rowoffset, Mobilebrand_position),
	Elevation = read_column_number(H, Dbtype, Rowoffset, Elevation_position),
	Usagetype = read_column_string(H, Dbtype, Rowoffset, Usagetype_position),
	#{
		country_short => Country_short,
		country_long => Country_long,
		region => Region,
		city => City,
		isp => Isp,
		latitude => Latitude,
		longitude => Longitude,
		domain => Domain,
		zipcode => Zipcode,
		timezone => Timezone,
		netspeed => Netspeed,
		iddcode => Iddcode,
		areacode => Areacode,
		weatherstationcode => Weatherstationcode,
		weatherstationname => Weatherstationname,
		mcc => Mcc,
		mnc => Mnc,
		mobilebrand => Mobilebrand,
		elevation => Elevation,
		usagetype => Usagetype
	}.

%% @private
read_string(H, StartPos) ->
	case file:pread(H, StartPos, 1) of
		eof ->
			erlang:error({eof, [StartPos, 1]});
		{ok, LenRaw} ->
			Len = binary:decode_unsigned(LenRaw, little),
			case file:pread(H, StartPos + 1, Len) of
				eof ->
					erlang:error({eof, [StartPos - 1, Len]});
				{ok, Data} ->
					Data
			end
	end.

%% @private
read_uint(H, StartPos, Len) ->
	case file:pread(H, StartPos - 1, Len) of
		eof ->
			erlang:error({eof, [StartPos - 1, Len]});
		{ok, Data} ->
			binary:decode_unsigned(Data, little)
	end.

%% @private
read_uint8(H, StartPos) ->
	read_uint(H, StartPos, 1).

%% @private
read_uint32(H, StartPos) ->
	read_uint(H, StartPos, 4).

%% @private
read_uint128(H, StartPos) ->
	read_uint(H, StartPos, 16).

%% @private
round(Number, Precision) ->
	P = math:pow(10, Precision),
	round(Number * P) / P.

%% @private
searchtree(_H, _Ipnum, _Dbtype, Low, High, _BaseAddr, _Colsize, _Iptype) when Low > High ->
	false;
searchtree(H, Ipnum, Dbtype, Low, High, BaseAddr, Colsize, Iptype=ipv4) ->
	Mid = ((Low + High) bsr 1),
	OffsetA = BaseAddr + (Mid * Colsize),
	OffsetB = OffsetA + Colsize,
	IpA = read_uint32(H, OffsetA),
	IpB = read_uint32(H, OffsetB),
	case Ipnum >= IpA andalso Ipnum < IpB of
		true ->
			{true, read_record(H, Dbtype + 1, OffsetA)};
		false ->
			case Ipnum < IpA of
				true ->
					searchtree(H, Ipnum, Dbtype, Low, Mid - 1, BaseAddr, Colsize, Iptype);
				false ->
					searchtree(H, Ipnum, Dbtype, Mid + 1, High, BaseAddr, Colsize, Iptype)
			end
	end;
searchtree(H, Ipnum, Dbtype, Low, High, BaseAddr, Colsize, Iptype=ipv6) ->
	Mid = ((Low + High) bsr 1),
	OffsetA = BaseAddr + (Mid * Colsize),
	OffsetB = OffsetA + Colsize,
	IpA = read_uint128(H, OffsetA),
	IpB = read_uint128(H, OffsetB),
	case Ipnum >= IpA andalso Ipnum < IpB of
		true ->
			{true, read_record(H, Dbtype + 1, OffsetA + 12)};
		false ->
			case Ipnum < IpA of
				true ->
					searchtree(H, Ipnum, Dbtype, Low, Mid - 1, BaseAddr, Colsize, Iptype);
				false ->
					searchtree(H, Ipnum, Dbtype, Mid + 1, High, BaseAddr, Colsize, Iptype)
			end
	end.

%% @private
to_ipaddr(List) when is_list(List) ->
	List;
to_ipaddr(Binary) when is_binary(Binary) ->
	binary:bin_to_list(Binary);
to_ipaddr(Tuple) when tuple_size(Tuple) == 4 orelse tuple_size(Tuple) == 8 ->
	inet:ntoa(Tuple).
