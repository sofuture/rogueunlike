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

-define(RUWORKER(Name),
    {Name, {Name, start_link, []}, permanent, 2000, worker, [Name]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MobsServer = ?RUWORKER(ru_mobs),
    CharServer = ?RUWORKER(ru_char),
    ConsoleServer = ?RUWORKER(ru_console),
    InputServer = ?RUWORKER(ru_input),
    WorldServer = ?RUWORKER(ru_world),
    StateServer = ?RUWORKER(ru_state),
    Children = [MobsServer, CharServer, ConsoleServer, 
        InputServer, WorldServer, StateServer],
    RestartStrategy = {one_for_one, 2, 10},
    {ok, {RestartStrategy, Children}}.

