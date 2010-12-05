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

-module(ru_world).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include_lib("stdlib/include/qlc.hrl").

-include("encurses.hrl").
-include("ru.hrl").

-export([start/0, world_loop/1, database_test/0, redraw/1, init/1]).

-export([square_has/2, square_add/2, square_sub/2, get_square/1, 
         save_square/1, hero_location/0, tick/0, mob_location/1]).

%% ============================================================================
%% Application API
%% ============================================================================

database_test() ->
    ?MODULE ! {database_test, self()},
    ?WAITFOROK.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

hero_location() ->
    ?MODULE ! {find_hero, self()},
    ?WAITFORRET.

mob_location(MobRef) ->
    ?MODULE ! {find_mob, self(), MobRef},
    ?WAITFORRET.

init(ConsHeight) ->
    ?MODULE ! {init, ConsHeight}.

get_square(Location) ->
    ?MODULE ! {get_square, self(), Location},
    ?WAITFORRET.

tick() ->
    ok.

%% ============================================================================
%% Application Behavior
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, world_loop, [#world_state{}])).

world_loop(State) ->
    receive
        {init, ConsHeight} -> 
            init_db(),
            {MaxX, MaxY} = encurses:getmaxxy(),
            WinHeight = MaxY - (ConsHeight + 1),
            Win = create_window(WinHeight, MaxX),
            world_loop(State#world_state{
                win = Win, height = WinHeight, width = MaxX});

        {database_test, Caller} ->
            create_test_world(),
            Caller ! ok,
            world_loop(State);

        {find_hero, Caller} ->
            Caller ! {ok, find_hero()},
            world_loop(State);

        {find_mob, Caller, MobRef} ->
            Caller ! {ok, find_mob(MobRef)},
            world_loop(State);

        {redraw, _Reason} ->
            Win = State#world_state.win,
            encurses:erase(Win),
            draw_world(Win),
            world_loop(State);

        {get_square, Caller, Location} ->
            Caller ! {ok, get_world_square(Location)},
            world_loop(State);

        {exit, _} ->
            ok;

        _ ->
            world_loop(State)
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

create_window(Height, Width) ->
    encurses:curs_set(?CURS_INVISIBLE),
    Win = encurses:newwin(Width, Height, 0, 0),
    encurses:refresh(Win),
    Win.

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

get_world_square(Location) ->
    Trans = fun() -> mnesia:read(world, Location) end,
    {atomic, [Square]} = mnesia:transaction(Trans),
    Square.

find_hero() ->
    Q = qlc:q([X || 
            X = #world{stuff=Stuff} <- mnesia:table(world),
            proplists:get_bool(hero, Stuff)]),
    F = fun() -> qlc:eval(Q) end,
    case mnesia:transaction(F) of
        {atomic, [Square]} -> Square;
        _ -> nil
    end.

find_mob(MobRef) ->
    FindMe = fun(Elem) ->
        case is_record(Elem, mob) of
            true -> Elem#mob.ref =:= MobRef;
            _ -> false
        end
    end,
    Q = qlc:q([X ||
            X = #world{stuff=Stuff} <- mnesia:table(world),
            case proplists:lookup_all(mob, Stuff) of
                [] -> false;
                List -> lists:any(FindMe, List)
            end]),
    F = fun() -> qlc:eval(Q) end,
    case mnesia:transaction(F) of
        {atomic, [Square]} -> 
            [Mob] = lists:filter(FindMe, Square#world.stuff),
            {Square, Mob};
        _ -> nil
    end.

save_square(Square) ->
    Trans = fun() -> mnesia:write(Square) end,
    {atomic, _} = mnesia:transaction(Trans),
    Square.

square_char(Stuff) ->
    PriElement = fun(Elem) -> draw_pref(Elem) end,
    PrefList = lists:map(PriElement, Stuff),
    {_Prio, BestChar} = lists:min(PrefList), 
    BestChar.

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

square_has(Square, mob) ->
    FindMob = fun(Elem) -> is_record(Elem, mob) end,
    lists:any(FindMob, Square#world.stuff);
square_has(Square, Thing) when is_record(Thing, mob) 
        andalso is_record(Square, world) ->
    FindMe = fun(Elem) -> Thing#mob.ref =:= Elem#mob.ref end,
    lists:any(FindMe, Square#world.stuff);
square_has(Square, Thing) ->
    proplists:get_bool(Thing, Square#world.stuff).

square_add(Square, []) ->
    Square;
square_add(Square, Things) when is_list(Things) ->
    [Head|Tail] = Things,
    square_add(square_add(Square, Head), Tail);
square_add(Square, Thing) ->
    Square#world{ stuff = [Thing | Square#world.stuff]}.

square_sub(Square, []) ->
    Square;
square_sub(Square, Things) when is_list(Things) ->
    [Head|Tail] = Things,
    square_sub(square_sub(Square, Head), Tail);
square_sub(Square, Thing) ->
    NotThing = fun(Elem) -> Elem =/= Thing end,
    Square#world{ stuff = lists:filter(NotThing, Square#world.stuff)}.

create_test_world() ->
    save_squares(room_with_door(0,0,10,4,{9,1})),
    save_squares(room_with_door(3,3,7,4,{6,3})),
    save_squares(room_with_door(9,0,20,10,{9,1})).

save_squares(Squares) ->
    Ins = fun(Elem) -> mnesia:write(Elem) end,
    Trans = fun() -> lists:foreach(Ins, Squares) end,
    mnesia:transaction(Trans),
    ok.

%% ============================================================================
%% Mnesia management
%% ============================================================================

init_db() ->
    case is_fresh_startup() of 
        true -> 
            case mnesia:system_info(is_running) of
                yes -> 
                    error_logger:tty(false),
                    mnesia:stop(),
                    error_logger:tty(true);
                _ -> ok
            end,
            mnesia:create_schema(node()),
            mnesia:start(),
            mnesia:create_table(world, 
                [{disc_copies, []}, {attributes, record_info(fields, world)}]);
        {exists, Tables} ->
            ok = mnesia:wait_for_tables(Tables, 20000)
    end,
    ok.

is_fresh_startup() ->
    Node = node(),
    case mnesia:system_info(is_running) of
        yes ->
            case mnesia:system_info(tables) of
                [schema] -> true;
                Tbls ->
                    case mnesia:table_info(schema, cookie) of
                        {_, Node} -> {exists, Tbls};
                        _ -> true
                    end
            end;
        _ ->
            mnesia:start(),
            is_fresh_startup()
    end.

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
            {4000, $.};
        door ->
            {5000, $+};
        wall ->
            {6000, $#};
        wall_floating -> 
            {6000, $#};
        wall_l ->
            {6000, ?ACS_HLINE};
        wall_b ->
            {6000, ?ACS_VLINE};
        wall_bl ->
            {6000, ?ACS_URCORNER};
        wall_r ->
            {6000, ?ACS_HLINE};
        wall_rl ->
            {6000, ?ACS_HLINE};
        wall_rb ->
            {6000, ?ACS_ULCORNER};
        wall_rbl -> 
            {6000, ?ACS_TTEE};
        wall_t ->
            {6000, ?ACS_VLINE};
        wall_tl ->
            {6000, ?ACS_LRCORNER};
        wall_tb ->
            {6000, ?ACS_VLINE};
        wall_tbl -> 
            {6000, ?ACS_RTEE};
        wall_tr ->
            {6000, ?ACS_LLCORNER};
        wall_trl ->
            {6000, ?ACS_BTEE};
        wall_trb -> 
            {6000, ?ACS_LTEE};
        wall_trbl ->
            {6000, ?ACS_PLUS};

        _ ->
            {10000, $\s}

    end.

%% ============================================================================
%% Map Generation
%% ============================================================================

row(X, Y, N, Type) ->
    [#world{loc={I,Y}, stuff=Type} ||
        I <- lists:seq(X, X+N-1)].

col(X, Y, N, Type) ->
    [#world{loc={X,J}, stuff=Type} ||
        J <- lists:seq(Y, Y+N-1)].

grid(X, Y, 1, J, Type) ->
    col(X, Y, J, Type);
grid(X, Y, I, J, Type) ->
    col(X, Y, J, Type) ++
    grid(X + 1, Y, I - 1, J, Type).

room(X, Y, I, J) ->
    row(X, Y, I, [wall]) ++ % top
    row(X, Y + J - 1, I, [wall]) ++ % bottom
    col(X, Y + 1, J - 2, [wall]) ++ % left
    col(X + I - 1, Y + 1, J - 2, [wall]) ++ % right
    grid(X + 1, Y + 1, I - 2, J - 2, [walkable]).

room_with_door(X, Y, I, J, {DoorX, DoorY}) ->
    [#world{loc={DoorX, DoorY}, stuff=[door]} | 
        [World || World <- room(X, Y, I, J),
            World#world.loc /= {DoorX, DoorY}]
    ].

