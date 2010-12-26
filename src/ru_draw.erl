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

-module(ru_draw).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include_lib("stdlib/include/qlc.hrl").
-include_lib("deps/encurses/include/encurses.hrl").
-include("ru.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([start_link/0]).
-export([draw/1, cleanup/0, init/0]).

-record(state, {worldwin, conswin}).

%% ============================================================================
%% Module API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    gen_server:call(?MODULE, init).

draw(Reason) ->
    gen_server:call(?MODULE, {draw, Reason}).

cleanup() ->
    gen_server:call(?MODULE, cleanup).

%% ============================================================================
%% gen_server Behaviour
%% ============================================================================

init(_) ->
    {ok, #state{}}.

handle_call({draw, _Reason}, _From, State) ->
    {reply, ok, do_draw(State)};
handle_call(init, _From, State) ->
    {reply, do_init(), State};
handle_call(cleanup, _From, State) ->
    {reply, do_cleanup(), State}.

handle_cast({init, ConsHeight}, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================
    
do_init() ->
    encurses:initscr(),
    encurses:keypad(0, true),
    encurses:noecho(),
    ok.

do_cleanup() ->
    encurses:erase(),
    encurses:refresh(),
    encurses:endwin(),
    ok.

do_draw(State) ->
    State#state{ %worldwin = draw_world(State#state.worldwin),
        conswin = draw_console(State#state.conswin) }.

draw_world(Win) ->
    Q = qlc:q([X || X <- mnesia:table(world)]),
    F = fun() -> qlc:eval(Q) end,
    {atomic, World} = mnesia:transaction(F),
    {WorldWidth, WorldHeight} = bounding_dimensions(World),
    {DrawX, DrawY} = ru_util:centering_coords(WorldWidth, WorldHeight),
    DrawF = fun(Spot) ->
        Char = square_char(Spot#world.stuff),
        {LocX, LocY} = Spot#world.loc,
        encurses:mvwaddch(Win, DrawX+LocX, DrawY+LocY, Char)
    end,
    lists:foreach(DrawF, World),
    encurses:refresh(Win),
    ok.

draw_console(Win) ->
    ok.

bounding_dimensions(World) ->
    FXs = fun(Elem) ->
        {X, _} = Elem#world.loc,
        X
    end,
    FYs = fun(Elem) ->
        {_, Y} = Elem#world.loc,
        Y
    end,
    MaxX = lists:max(lists:map(FXs, World)),
    MaxY = lists:max(lists:map(FYs, World)),
    {MaxX + 1, MaxY + 1}.

square_char(Stuff) ->
    PriElement = fun(Elem) -> draw_pref(Elem) end,
    PrefList = lists:map(PriElement, Stuff),
    {_Prio, BestChar} = lists:min(PrefList),
    BestChar.

draw_pref(Thing) ->
    case Thing of
        
        %% mobs
        hero ->
            {0, $@};

        #mob{} = MobRec ->
            Type = MobRec#mob.type,
            case Type of
                dog ->
                    {1, $d};
                zombie ->
                    {1, $Z};
                _ ->
                    {1, $?}
            end;

        %% stuff
        fountain -> 
            {1000, $U};
        opendoor ->
            {1001, $|};

        %% infrastructureystuff
        walkable -> 
            {4000, $\s};
        door ->
            {5000, $+};

        wall_ulcorner ->
            {6000, ?ACS_ULCORNER};
        wall_urcorner ->
            {6000, ?ACS_URCORNER};
        wall_llcorner ->
            {6000, ?ACS_LLCORNER};
        wall_lrcorner ->
            {6000, ?ACS_LRCORNER};
        
        wall_cross -> 
            {6000, ?ACS_PLUS};

        wall_ttee ->
            {6000, ?ACS_TTEE};
        wall_btee ->
            {6000, ?ACS_BTEE};
        wall_ltee ->
            {6000, ?ACS_LTEE};
        wall_rtee ->
            {6000, ?ACS_RTEE};

        wall_hline ->
            {6000, ?ACS_HLINE};
        wall_vline ->
            {6000, ?ACS_VLINE};

        wall ->
            {6001, $#};
        wall_floating -> 
            {6001, $#};

        _ ->
            {10000, $\s}

    end.

