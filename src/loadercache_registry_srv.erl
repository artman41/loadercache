-module(loadercache_registry_srv).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([all/0]).
-export([whereis_name/1, register_name/2, unregister_name/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

-define(TABLE, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all() ->
    ets:tab2list(?TABLE).

-spec whereis_name(Name :: any()) -> pid() | undefined.
whereis_name(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{Name, Pid}] ->
            Pid;
        _ ->
            undefined
    end.

-spec register_name(Name :: any(), pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?MODULE, {register, {Name, Pid}}).

-spec unregister_name(Name :: any()) -> true.
unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% gen_server.

init([]) ->
    erlang:process_flag(trap_exit, true),
    ets:new(?TABLE, [protected, named_table]),
    {ok, #state{}}.

handle_call({register, KV}, _From, State) ->
    Ret = 
        case ets:insert_new(?TABLE, KV) of
            true -> yes;
            false -> no
        end,
    {reply, Ret, State};
handle_call({unregister, Name}, _From, State) ->
    true = ets:delete(?TABLE, Name),
    {reply, true, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_all_registered_processes(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% INTERNAL
stop_all_registered_processes() ->
    stop_all_registered_processes_(ets:first(?TABLE)).

stop_all_registered_processes_('$end_of_table') ->
    ok;
stop_all_registered_processes_(Name) ->
    Pid = ets:lookup_element(?TABLE, Name, 2),
    erlang:exit(Pid, {shutdown, registry_dead}),
    ets:delete(?TABLE, Name),
    stop_all_registered_processes_(ets:first(?TABLE)).