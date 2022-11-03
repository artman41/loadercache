-module(loadercache_cache_sup_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(CacheConfig) ->
	supervisor:start_child(?MODULE, [CacheConfig]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one, 
        intensity => 1,
        period => 5
    },
    Children = [
        #{
            id => loadercache_cache_sup,
            start => {loadercache_cache_sup, start_link, []},
            restart => transient,
            shutdown => 2000,
            type => supervisor
        }
    ],
    {ok, {SupFlags, Children}}.