# ip2locationdata

[![Hex.pm](https://img.shields.io/hexpm/v/ip2locationdata.svg)](https://hex.pm/packages/ip2locationdata)

Downloads and updates the [IP2Location Databases](https://www.ip2location.com/download) and/or [IP2Location LITE Databases](http://lite.ip2location.com/database) for Erlang and Elixir.

Designed to be used by itself or alongside other applications like:

* [nazipov/ip2location-elixir](https://github.com/nazipov/ip2location-elixir)
* [danielgracia/ip2location-elixir](https://github.com/danielgracia/ip2location-elixir)
* [ip2location/ip2location-erlang](https://github.com/ip2location/ip2location-erlang)

## Installation

Add `ip2locationdata` to your project's dependencies in `mix.exs`

```elixir
defp deps do
  [
    {:ip2locationdata, "~> 0.0.4"}
  ]
end
```

Add `ip2locationdata` to your project's dependencies in your `Makefile` for [`erlang.mk`](https://github.com/ninenines/erlang.mk) or the following to your `rebar.config`

```erlang
{deps, [
  {ip2locationdata, ".*", {git, "git://github.com/potatosalad/erlang-ip2locationdata.git", {branch, "master"}}}
]}.
```

## Configuration

You **MUST** specify `credentials` and `packages` you wish to track as part of your application configuration.

For example (using `mix`):

```elixir
config :ip2locationdata,
  credentials: %{ login: "USERNAME", password: "PASSWORD" },
  packages: [
    %{ id: :DB11LITEBINIPV6, productcode: "DB11LITEBINIPV6" }
  ]
```

This will automatically check for updates once a day by default.

The defaults for all configuration options are:

```elixir
config :ip2locationdata,
  autoupdate: 86400, # 24 hours
  credentials: :undefined,
  endpoints: %{
    download: "https://www.ip2location.com/download",
    info: "https://www.ip2location.com/download-info"
  },
  keep_releases: 2,
  logger: false,
  packages: []
```

## Usage

Simple querying may be done using `ip2locationdata_adapter_bin`:

```elixir
iex> {:ok, filename} = :ip2locationdata.package_get(:DB11LITEBINIPV6)
iex> {:ok, adapter} = :ip2locationdata_adapter_bin.read(filename)
iex> :ip2locationdata_adapter_bin.query(adapter, "8.8.8.8")
{true,
 %{areacode: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   city: "Mountain View", country_long: "United States", country_short: "US",
   domain: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   elevation: 0.0,
   iddcode: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   isp: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   latitude: 37, longitude: -122,
   mcc: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   mnc: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   mobilebrand: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   netspeed: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   region: "California", timezone: "-07:00",
   usagetype: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   weatherstationcode: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   weatherstationname: "This parameter is unavailable for selected data file. Please upgrade the data file.",
   zipcode: "94043"}}
```

For more advanced usage, there is an event handler that can be used to watch for any changes to database files (Erlang example):

```erlang
-module(example_ip2locationdata_listener).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = ip2locationdata_event:add_handler(ip2locationdata_event_handler, self()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'$ip2locationdata-event', {package, announce, ID, Filename}}, State) ->
    %% Handle changes to the database identified by 'ID' and stored at 'Filename'
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

See `ip2locationdata_logger` for more examples.

## License

[Mozilla Public License, Version 2.0](https://www.mozilla.org/en-US/MPL/2.0/)
