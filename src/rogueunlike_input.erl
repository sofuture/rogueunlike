%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_input).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([input_loop/0, key_loop/0]).

%% ============================================================================
%% Module API
%% ============================================================================

input_loop() ->
    true = register(keyreader, 
        spawn(?MODULE, key_loop, [])),
    
    BasicInput = fun(Input) ->
            case Input of
                $q -> 
                    suicide ! {exit, die};
                _ ->
                    logic ! {dosomething, nil}
            end
    end,

    recv_loop(BasicInput).

%% ============================================================================
%% Internal Functions
%% ============================================================================

recv_loop(Mode) ->
    receive
        {exit, _} -> 
            ok;

        {mode, NewMode} ->
            recv_loop(NewMode);

        {input, Input} ->
            Mode(Input),
            recv_loop(Mode);
            
        _ -> 
            recv_loop(Mode)
    end.

key_loop() ->
    input ! {input, cecho:getch()},
    key_loop().

