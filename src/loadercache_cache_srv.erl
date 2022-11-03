-module(loadercache_cache_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1, stop/1]).
-export([get/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    cache_config :: loadercache_cache:cache_config()
}).

%% API.

-spec start_link(CacheConfig :: cacheloader_cache:cache_config()) -> {ok, pid()}.
start_link(CacheConfig) ->
    gen_server:start_link({via, loadercache_registry_srv, loadercache_cache:name(CacheConfig)}, ?MODULE, [CacheConfig], []).

stop(CacheName) ->
    gen_server:stop({via, loadercache_registry_srv, CacheName}).

get(CacheName, Key) ->
    gen_server:call({via, loadercache_registry_srv, CacheName}, {get, Key}).

%% gen_server.

init([CacheConfig]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{cache_config = CacheConfig}}.

handle_call({get, Key}, _From, State = #state{cache_config = CacheConfig}) ->
    Ret = loadercache_cache:get(CacheConfig, Key),
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{cache_config = CacheConfig}) ->
    gen:unregister_name({via, loadercache_registry_srv, loadercache_cache:name(CacheConfig)}),
    gen_server:stop(hd(get('$ancestors')), Reason, infinity),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
