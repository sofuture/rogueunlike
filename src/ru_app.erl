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

-module(ru_app).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case ru_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) ->
    ok.
