-module(loadercache_watcher_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([opts_updated/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	cache_name :: any()
}).

%% API.

-spec start_link(any()) -> {ok, pid()}.
start_link(CacheName) ->
	gen_server:start_link(?MODULE, [CacheName], []).

opts_updated(WatcherSrv) ->
	gen_server:cast(WatcherSrv, opts_updated).

%% gen_server.

init([CacheName]) ->
	% Opts = loadercache_cache:get_opts(CacheName),
	{ok, #state{cache_name = CacheName}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
