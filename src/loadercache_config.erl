-module(loadercache_config).

-export([get_caches/0]).

get_caches() ->
    get_value(caches, []).

%% INTERNAL

get_value(Key) ->
    application:get_env(loadercache, Key).

get_value(Key, Default) ->
    case get_value(Key) of
        {ok, V} -> V;
        undefined -> Default
    end.