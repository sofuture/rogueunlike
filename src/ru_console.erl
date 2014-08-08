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

-module(ru_console).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([message/1, get_lines/0]).

-record(state, {
        win = nil,
        lines = [],
        statline = [],
        height = 0,
        width = 0}).

%% ============================================================================
%% Application API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

message(Message) ->
    ?CAST({message, Message}).

get_lines() ->
    ?CALL(get_lines).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_lines, _From, State) ->
    {reply, State#state.lines, State}.

handle_cast({message, Message}, State) ->
    {noreply, do_message(State, Message)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

do_message(State, Message) ->
    NewState = State#state{ lines = [Message | State#state.lines] },
    %ru_draw:draw(message),
    NewState.

