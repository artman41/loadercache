-module(loadercache_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([bootstrap_init/0]).
-export([init/1]).

-export([insert_tab/2, lookup_tab/1, delete_tab/1]).

-define(TABLE, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

insert_tab(Key, Tab) ->
    case ets:lookup(?TABLE, Key) of
        [] -> 
            ok;
        [{Key, T}] -> 
            case ets:info(T, id) of
                T -> 
                    erlang:error(badarg);
                undefined ->
                    ok
            end
    end,
    ets:insert(?TABLE, {Key, Tab}).

lookup_tab(Key) ->
    ets:lookup_element(?TABLE, Key, 2).

delete_tab(Key) ->
    ets:delete(?TABLE, Key).

init([]) ->
    ets:new(?TABLE, [public, named_table]),
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        #{
            id => loadercache_registry_srv,
            start => {loadercache_registry_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        #{
            id => loadercache_cache_sup_sup,
            start => {loadercache_cache_sup_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor
        },
        #{
            id => loadercache_bootstrap_init,
            start =>  {proc_lib, start, [?MODULE, bootstrap_init, []]},
            restart => temporary,
            shutdown => 2000,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.

bootstrap_init() ->
    proc_lib:init_ack({ok, self()}),
    [loadercache_cache_sup_sup:start_child(CacheConfig) || CacheConfig <- loadercache_cache:from_config()].