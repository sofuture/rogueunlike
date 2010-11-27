%% ============================================================================
%% Rogueunlike 0.30.0
%%
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(ru_mobs).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("ru.hrl").

-export([start/0]).
-export([tick/0, add/1, update/1]).
-export([state_loop/1]).

%% ============================================================================
%% Module API
%% ============================================================================

tick() ->
    ?MODULE ! {tick, self()},
    ?WAITFOROK.

add(#mob{} = Mob) ->
    ?MODULE ! {add, self(), Mob},
    ?WAITFOROK.

update(#mob{} = Mob) ->
    ?MODULE ! {update, self(), Mob},
    ?WAITFOROK.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, state_loop, [[]])).

state_loop(State) ->
    receive
        {tick, Caller} ->
            tick(State),
            Caller ! ok,
            state_loop(State);

        {add, Caller, Mob} ->
            Caller ! ok,
            state_loop([Mob | State]);

        {update, Caller, Mob} ->
            Caller ! ok,
            state_loop(update_mob(Mob, State));
        
        {exit, _} -> 
            ok;

        _ -> 
            state_loop(State)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

tick([]) ->
    ok;
tick([Head | Tail] = _MobList) ->
    F = Head#mob.func,
    case F of
        nil -> ok;
        _ -> F(tick, Head)
    end,
    tick(Tail).

update_mob(Mob, State) ->
    MobRef = Mob#mob.ref,
    OtherFilter = fun(Elem) -> Elem#mob.ref =/= MobRef end,
    Others = lists:filter(OtherFilter, State),
    [Mob | Others].
