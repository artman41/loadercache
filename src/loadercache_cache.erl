-module(loadercache_cache).

-export([from_config/0]).
-export([new/2, new/3]).
-export([name/1]).
-export([create_tabs/1]).
-export([get_opts/1, set_opts/2]).
-export([fetch/2, get/2]).

-record(cache_config, {
    cache_name :: any(),
    loader :: cache_loader() | undefined,
    opts :: cache_opts()
}).

-opaque cache_loader() :: fun((Key :: any()) -> {ok, any()} | {error, any()}).
-opaque cache_opts() :: list(cache_opt()).
-opaque cache_opt() :: {entry_lifetime, pos_integer() | infinity} %% How long does a key stay in the cache before it is removed?
                     | {loadfail_lifetime, pos_integer() | infinity} %% How long do we remember that a cache_load failed?
                     | {max_entries, pos_integer() | infinity}.
-opaque cache_config() :: #cache_config{}.

-export_type([cache_loader/0, cache_opts/0, cache_opt/0, cache_config/0]).

from_config() ->
    from_config_(loadercache_config:get_caches()).

-spec new(CacheName :: any(), Loader :: undefined | {module(), atom()} | cache_loader()) -> cache_config().
new(CacheName, Loader) ->
    new(CacheName, Loader, []).

-spec new(CacheName :: any(), Loader :: undefined | {module(), atom()} | cache_loader(), Opts :: cache_opts()) -> cache_config().
new(CacheName, Loader, Opts) when Loader =:= undefined orelse (is_tuple(Loader) andalso tuple_size(Loader) =:= 2 orelse is_function(Loader, 1)) andalso is_list(Opts)->
    #cache_config{
        cache_name = CacheName, 
        loader = Loader, 
        opts = Opts
    }.

name(#cache_config{cache_name = CacheName}) ->
    CacheName.

create_tabs(#cache_config{cache_name = CacheName, opts = Opts, loader = Loader}) ->
    Tables = [
        {CacheName, public},
        {{cachemiss, CacheName}, public, Loader =/= undefined},
        {{opts, CacheName}, public}
    ],
    CreatedTables = 
        [begin
            {TabName, TabPublicity} =
                case TabConfig of
                    {TabName_, TabPublicity_, _} ->
                        {TabName_, TabPublicity_};
                    _ ->
                        TabConfig
                end,
            Tab = ets:new(table, [TabPublicity]),
            loadercache_sup:insert_tab(TabName, Tab),
            {TabName, Tab}
        end || TabConfig <- Tables, tuple_size(TabConfig) =:= 2 orelse element(3, TabConfig) =:= true],
    ets:insert(proplists:get_value({opts, CacheName}, CreatedTables), Opts).

set_opts(CacheName, Opts) when is_list(Opts) ->
    Keys = [entry_lifetime, loadfail_lifetime, max_entries],
    ets:insert(loadercache_sup:lookup_tab({opts, CacheName}), [Opt || Opt = {Key, _} <- Opts, lists:keymember(Key, 1, Keys)]),
    CacheSup = ets:info(loadercache_sup:lookup_tab({opts, CacheName}), owner),
    {_Id, WatcherSrv, _, _} = lists:keyfind(loadercache_watcher_srv, 1, supervisor:which_children(CacheSup)),
    loadercache_watcher_srv:opts_updated(WatcherSrv),
    ok.

get_opts(CacheName) ->
    Keys = [entry_lifetime, loadfail_lifetime, max_entries],
    lists:flatten([ets:lookup(loadercache_sup:lookup_tab({opts, CacheName}), Key) || Key <- Keys]).

fetch(CacheName, Key) ->
    case ets:lookup(loadercache_sup:lookup_tab(CacheName), Key) of
        [] -> 
            undefined;
        [{Key, V}] ->
            {ok, V}
    end.

get(#cache_config{cache_name = CacheName, loader = undefined}, Key) ->
    fetch(CacheName, Key);
get(CacheConfig = #cache_config{cache_name = CacheName}, Key) ->
    case fetch(CacheName, Key) of
        undefined ->
            get_(CacheConfig, Key);
        Value = {ok, _} ->
            Value
    end.

%% Internal

-spec from_config_(list(#{cache_name => any(), loader => undefined | {module(), atom()}, opts => cache_opts()})) -> list(cache_config()).
from_config_([]) ->
    [];
from_config_([Config = #{cache_name := CacheName}|Tail]) ->
    CacheConfig0 = #cache_config{cache_name = CacheName},
    Loader = 
        case maps:find(loader, Config) of
            {ok, {M, F}} when is_atom(M) andalso is_atom(F) ->
                case load_and_check_function(M, F, 1) of
                    true ->
                        {ok, fun M:F/1};
                    false ->
                        {error, io_lib:format("~p:~p/1 doesn't exist!", [M, F])}
                end;
            {ok, FoundLoader} ->
                {error, io_lib:format("~p is an invalid Loader!", [FoundLoader])};
            error ->
                {ok, undefined}
        end,
    Opts = 
        case maps:find(opts, Config) of
            {ok, FoundOpts} when is_list(FoundOpts) ->
                {ok, FoundOpts};
            {ok, FoundOpts} ->
                {error, io_lib:format("~p is invalid Opts!", [FoundOpts])};
            error ->
                {ok, []}
        end,
    Acc = from_config_(Tail),
    case {Loader, Opts} of
        {{ok, V_Loader}, {ok, V_Opts}} ->
            CacheConfig1 =
                CacheConfig0#cache_config{
                    loader = V_Loader, 
                    opts = V_Opts
                },
            [CacheConfig1 | Acc];
        Other ->
            ErrMsg =
                case Other of
                    {{error, Msg}, _} -> Msg;
                    {_, {error, Msg}} -> Msg
                end,
            error_logger:error_msg("Failed to parse config for Cache ~p, ~s", [CacheName, ErrMsg]),
            Acc
    end.

load_and_check_function(Mod, Fun, Arity) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            erlang:function_exported(Mod, Fun, Arity);
        _ ->
            false
    end.

get_(#cache_config{cache_name = CacheName, loader = Loader}, Key) ->
    CacheMiss = loadercache_sup:lookup_tab({cachemiss, CacheName}),
    case ets:lookup(CacheMiss, Key) of
        [] ->
            case Loader(Key) of
                {ok, Value} ->
                    ets:insert(loadercache_sup:lookup_tab(CacheName), {Key, Value}),
                    {ok, Value};
                {error, _} ->
                    ets:insert(CacheMiss, {Key, os:timestamp()}),
                    undefined
            end;
        [{Key, _}] ->
            undefined
    end.