-module(loadercache_cache_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(CacheConfig) ->
	supervisor:start_link(?MODULE, [CacheConfig]).

init([CacheConfig]) ->
	loadercache_cache:create_tabs(CacheConfig),
	CacheName = loadercache_cache:name(CacheConfig),
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1,
        period => 5
    },
    Children = [
        #{
            id => loadercache_watcher_srv,
            start => {loadercache_watcher_srv, start_link, [CacheName]},
            restart => transient,
            shutdown => 2000,
            type => worker
        },
        #{
            id => loadercache_cache_srv,
            start => {loadercache_cache_srv, start_link, [CacheConfig]},
            restart => transient,
            shutdown => 2000,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.