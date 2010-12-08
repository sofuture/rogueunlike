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

-module(ru_text).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("ru.hrl").

-export([help_menu/0, about_menu/0]).

%% ============================================================================
%% Application API
%% ============================================================================

about_menu() ->
    ["About Rogueunlike",
     "-----------------",
     "Rogueunlike is not exactly (but sort of) unlike a roguelike game.",
     "",
     "It is/was/haswillbeen written by Jeff Zellner in a bunch of",
     "Erlang code/scribbles. All rights belong to J.R. 'Bob' Dobbs",
     "as well as Jeff Zellner and Aleister Crowley. Don't sue me. TIA.",
     "",
     "jeff.zellner\@gmail.com"].

help_menu() ->
    ["Help for Rogueunlike",
     "--------------------",
     "Commands:",
     " a - attack",
     " A - about window",
     " c - close",
     " o - open",
     " ? - help window"].
