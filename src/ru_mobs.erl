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
-export([tick/0, add/1]).

%% ============================================================================
%% Module API
%% ============================================================================

tick() ->
    ?MODULE ! {tick, tock}.

add(#mob{} = Mob) ->
    ?MODULE ! {add, self(), Mob},
    ?WAITFOROK.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(fun() -> state_loop([]) end)).

state_loop(State) ->
    receive
        {tick, _} ->
            tick(State),
            state_loop(State);

        {add, Caller, Mob} ->
            Caller ! ok,
            state_loop([Mob | State]);
        
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
        _ -> F(tick, Head#mob.ref)
    end,
    tick(Tail).

