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

-module(ru_sup).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MobsServer = {ru_mobs, {ru_mobs, start_link, []}, 
        permanent, 2000, worker, [ru_mobs]},
    CharServer = {ru_char, {ru_char, start_link, []}, 
        permanent, 2000, worker, [ru_char]},
    Children = [MobsServer, CharServer],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

