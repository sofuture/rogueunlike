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

-export([input_loop/0, key_loop/1]).
-export([script_mode/2,game_mode/2]).

%% ============================================================================
%% Module API
%% ============================================================================

input_loop() ->
    true = register(keyreader, 
        spawn(?MODULE, key_loop, [[]])),
    
    recv_loop(fun(_) -> ok end, #input{}).

%% ============================================================================
%% Internal Functions
%% ============================================================================


recv_loop(Mode, State) ->
    receive
        {exit, _} -> 
            ok;

        {mode, NewMode} ->
            %% TODO maybe default clear state here?
            recv_loop(NewMode, State);

        {input, Input} ->
            Mode(Input, State),
            recv_loop(Mode, State);

        {redraw, _Reason} ->
            recv_loop(Mode, State);
            
        _ -> 
            recv_loop(Mode, State)
    end.

key_loop(Buffer) ->
    cecho:noecho(),
    Char = cecho:getch(),
    NewBuf = case Char of
        27 -> [27];
        _ -> [Char | Buffer]
    end,
    RetChar = case lists:reverse(NewBuf) of
        %% this is crazy but oh fuckin well amirite??
        [27, 91, 49, 126] -> kp_nw;
        [27, 91, 52, 126] -> kp_sw;
        [27, 91, 53, 126] -> kp_ne;
        [27, 91, 54, 126] -> kp_se;
        [27, 91, 65] -> kp_n;
        [27, 91, 66] -> kp_s;
        [27, 91, 67] -> kp_e;
        [27, 91, 68] -> kp_w;
        [27, 91, 69] -> kp_center;
        [27, 91, 49] -> nil;
        [27, 91, 52] -> nil;
        [27, 91, 53] -> nil;
        [27, 91, 54] -> nil;
        [27, 91] -> nil;
        [27] -> nil;
        _ -> Char
    end,
    case RetChar of 
        nil -> ok;
        _ -> input ! {input, RetChar}
    end,
    key_loop(NewBuf).

%% ============================================================================
%% Input Modes
%% ============================================================================

script_mode(Input, _State) ->
    case Input of
        $q -> main ! {exit, die};
        _ -> script ! {dosomething, nil}
    end.

game_mode(Input, _State) ->
    case Input of
        $Q -> main ! {exit, die};
        kp_nw -> ok;
        kp_n -> ok;
        kp_ne -> ok;
        kp_w -> ok;
        kp_center -> ok;
        kp_e -> ok;
        kp_sw -> ok;
        kp_s -> ok;
        kp_se -> ok;
        _ -> console ! {msg, io_lib:format("~p",[Input])}
    end.

