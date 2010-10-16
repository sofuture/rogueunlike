%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_char).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([char_loop/1]).

%% ============================================================================
%% Application API
%% ============================================================================

char_loop(Char) ->
    receive
        {exit, _} -> 
            ok;

        _ -> 
            char_loop(Char)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

