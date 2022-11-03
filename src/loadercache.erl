-module(loadercache).

-export([
    all/0,
    fetch/2,
    get/2,
    create_cache/2, create_cache/3,
    delete_cache/1
]).

all() ->
    [Name || {Name, _Pid} <- loadercache_registry_srv:all()].

fetch(CacheName, Key) ->
    loadercache_cache:fetch(CacheName, Key).

get(CacheName, Key) ->
    loadercache_cache_srv:get(CacheName, Key).

create_cache(CacheName, Loader) ->
    CacheConfig = loadercache_cache:new(CacheName, Loader),
    create_cache_(CacheConfig).

create_cache(CacheName, Loader, Opts) ->
    CacheConfig = loadercache_cache:new(CacheName, Loader, Opts),
    create_cache_(CacheConfig).

delete_cache(CacheName) ->
    loadercache_cache_srv:stop(CacheName).

%% Internal

create_cache_(CacheConfig) ->
    loadercache_cache_sup_sup:start_child(CacheConfig).