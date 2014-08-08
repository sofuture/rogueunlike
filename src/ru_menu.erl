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

-module(ru_menu).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([create/1, clear/0, has_menu/0, get_lines/0]).

-record(state, {curtext=nil}).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Text) ->
    ?CAST({create, Text}).

clear() ->
    ?CAST(clear).

has_menu() ->
    ?CALL(has_menu).

get_lines() ->
    ?CALL(get_lines).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(_) ->
    {ok, #state{}}.

handle_call(has_menu, _From, State) ->
    {reply, State#state.curtext =/= nil, State};
handle_call(get_lines, _From, State) ->
    {reply, State#state.curtext, State}.

handle_cast({create, Text}, State) ->
    {noreply, State#state { curtext=Text }};
handle_cast(clear, State) ->
    {noreply, State#state { curtext=nil }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

