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

%% ============================================================================
%% Module API
%% ============================================================================


%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, state_loop, [[]])).

state_loop(State) ->
    receive
        {tick, _} ->
            tick(State),
            state_loop(State);

        {create, Mob} ->
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
    Head(tick),
    tick(Tail).

