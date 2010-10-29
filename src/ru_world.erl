%% ============================================================================
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

-include("cecho.hrl").
-include("ru.hrl").

-export([start/0, world_loop/1, database_test/0, redraw/1, init/0,
         hero_location/0]).

-export([square_has/2, square_add/2, square_sub/2, get_square/1, 
         save_square/1]).

%% ============================================================================
%% Module API
%% ============================================================================

start() ->
    true = register(?MODULE,
        spawn(?MODULE, world_loop, [nil])).

database_test() ->
    ?MODULE ! {database_test, test}.

redraw(Reason) ->
    ?MODULE ! {redraw, Reason}.

hero_location() ->
    ?MODULE ! {find_hero, self()},
    receive
        {ok, Location} -> Location;
        _ -> nil
    end.

init() ->
    ?MODULE ! {init}.

get_square(Location) ->
    ?MODULE ! {get_square, self(), Location},
    receive
        {ok, Square} -> Square;
        _ -> nil
    end.

world_loop(State) ->
    receive
        {init} -> 
            init_db(),
            world_loop(State);

        {database_test, _} ->
            create_test_world(),
            world_loop(State);

        {find_hero, Caller} ->
            Caller ! {ok, find_hero()},
            world_loop(State);

        {redraw, _Reason} ->
            draw_world(),
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

draw_world() ->
    Q = qlc:q([X || X <- mnesia:table(world)]),
    F = fun() -> qlc:eval(Q) end,
    {atomic, World} = mnesia:transaction(F),
    {WorldWidth, WorldHeight} = bounding_dimensions(World),
    {DrawX, DrawY} = ru_util:centering_coords(WorldWidth, WorldHeight),
    DrawF = fun(Spot) ->
        Char = square_char(Spot#world.stuff),
        {LocX, LocY} = Spot#world.loc,
        cecho:mvaddch(DrawY+LocY, DrawX+LocX, Char)
    end,
    lists:foreach(DrawF, World),
    cecho:refresh(),
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
        {atomic, [Square]} ->
            Square;
        _ ->
            nil
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

square_has(Square, Thing) ->
    proplists:get_bool(Thing, Square#world.stuff).

square_add(Square, []) ->
    Square;
square_add(Square, Things) when is_list(Things) ->
    [Head|Tail] = Things,
    square_add(square_add(Square, Head), Tail);
square_add(Square, Thing) ->
    Square#world{ stuff = [Thing | Square#world.stuff]}.

square_sub(Square, Thing) ->
    NotThing = fun(Elem) -> case Elem of Thing -> false; _ -> true end end,
    Square#world{ stuff = lists:filter(NotThing, Square#world.stuff)}.

create_test_world() ->
    Ins = fun(Elem) -> mnesia:write(Elem) end,
    Trans = fun() -> lists:foreach(Ins, test_world()) end,
    mnesia:transaction(Trans),
    ok.

test_world() ->
    %    0123456789
    %
    % 0  ########
    % 1  #......###
    % 2  ####.....#
    % 3     ###+###
    % 4     #.....#
    % 5     #.....#
    % 6     #######
    A = [#world{loc={0,0}, stuff=[wall]},
    #world{loc={1,0}, stuff=[wall]},
    #world{loc={2,0}, stuff=[wall]},
    #world{loc={3,0}, stuff=[wall]},
    #world{loc={4,0}, stuff=[wall]},
    #world{loc={5,0}, stuff=[wall]},
    #world{loc={6,0}, stuff=[wall]},
    #world{loc={7,0}, stuff=[wall]},
    #world{loc={8,0}, stuff=[wall]},
    #world{loc={9,0}, stuff=[wall]},
    #world{loc={0,1}, stuff=[wall]},
    #world{loc={1,1}, stuff=[hero, walkable]},
    #world{loc={2,1}, stuff=[walkable]},
    #world{loc={3,1}, stuff=[walkable]},
    #world{loc={4,1}, stuff=[walkable]},
    #world{loc={5,1}, stuff=[walkable]},
    #world{loc={6,1}, stuff=[walkable]},
    #world{loc={7,1}, stuff=[walkable]},
    #world{loc={8,1}, stuff=[walkable]},
    #world{loc={9,1}, stuff=[wall]},
    #world{loc={0,2}, stuff=[wall]},
    #world{loc={1,2}, stuff=[wall]},
    #world{loc={2,2}, stuff=[wall]},
    #world{loc={3,2}, stuff=[wall]},
    #world{loc={4,2}, stuff=[walkable]},
    #world{loc={5,2}, stuff=[walkable]},
    #world{loc={6,2}, stuff=[walkable]},
    #world{loc={7,2}, stuff=[walkable]},
    #world{loc={8,2}, stuff=[walkable]},
    #world{loc={9,2}, stuff=[wall]},
    #world{loc={3,3}, stuff=[wall]},
    #world{loc={4,3}, stuff=[wall]},
    #world{loc={5,3}, stuff=[wall]},
    #world{loc={6,3}, stuff=[door]},
    #world{loc={7,3}, stuff=[wall]},
    #world{loc={8,3}, stuff=[wall]},
    #world{loc={9,3}, stuff=[wall]},
    #world{loc={3,4}, stuff=[wall]},
    #world{loc={4,4}, stuff=[walkable]},
    #world{loc={5,4}, stuff=[walkable]},
    #world{loc={6,4}, stuff=[walkable]},
    #world{loc={7,4}, stuff=[walkable]},
    #world{loc={8,4}, stuff=[walkable]},
    #world{loc={9,4}, stuff=[wall]},
    #world{loc={3,5}, stuff=[wall]},
    #world{loc={4,5}, stuff=[walkable]},
    #world{loc={5,5}, stuff=[walkable]},
    #world{loc={6,5}, stuff=[walkable]},
    #world{loc={7,5}, stuff=[walkable]},
    #world{loc={8,5}, stuff=[walkable]},
    #world{loc={9,5}, stuff=[wall]},
    #world{loc={3,6}, stuff=[wall]},
    #world{loc={4,6}, stuff=[wall]},
    #world{loc={5,6}, stuff=[wall]},
    #world{loc={6,6}, stuff=[wall]},
    #world{loc={7,6}, stuff=[wall]},
    #world{loc={8,6}, stuff=[wall]},
    #world{loc={9,6}, stuff=[wall]}],
    generate_test_world().

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
        hero ->
            {0, $@};
        fountain -> 
            {80, $U};
        opendoor ->
            {390, $|};
        walkable -> 
            {400, $.};
        door ->
            {500, $+};
        wall ->
            {1000, $#};
        _ ->
            {10000, $\s}
    end.

%% ============================================================================
%% Map Generation
%% ============================================================================

%wall_across(X, Y, N) ->
%    [#world{loc={I,Y}, stuff=[wall]} || 
%        I <- lists:seq(X, X+N-1)].
%
%wall_down(X, Y, N) ->
%    [#world{loc={X,J}, stuff=[wall]} ||
%        J <- lists:seq(Y, Y+N-1)].

row(X, Y, N, Type) ->
    [#world{loc={I,Y}, stuff=Type} ||
        I <- lists:seq(X, X+N-1)].

col(X, Y, N, Type) ->
    [#world{loc={X,J}, stuff=Type} ||
        J <- lists:seq(Y, Y+N-1)].

%% *<* dalexander has joined channel #lmit
%% <dalexander> grids?
grid(X, Y, 1, J, Type) ->
    col(X, Y, J, Type);
grid(X, Y, I, J, Type) ->
    col(X, Y, J, Type) ++
    grid(X + 1, Y, I - 1, J, Type).

% returns a list of world records
% x,y is the top left corner
% i,j are the wall lengths
room(X, Y, I, J) ->
    row(X, Y, I, [wall]) ++ % top
    row(X, Y + J - 1, I, [wall]) ++ % bottom
    col(X, Y + 1, J - 2, [wall]) ++ % left
    col(X + I - 1, Y + 1, J - 2, [wall]) ++ % right
    grid(X + 1, Y + 1, X + I - 2, Y + J - 2, [walkable]).

room_with_door(X, Y, I, J, {DoorX, DoorY}) ->
    [#world{loc={DoorX, DoorY}, stuff=[door]} | 
        [World || World <- room(X, Y, I, J),
            World#world.loc /= {DoorX, DoorY}]
    ].


hallway({X1, Y1}, {X2, Y2}) when (Y1 == Y2) and (X2 > X1) ->
    row(X1 + 1, Y1 - 1, (X2 - X1) - 1, [wall]) ++
    row(X1 + 1, Y1,     (X2 - X1) - 1, [walkable]) ++
    row(X1 + 1, Y1 + 1, (X2 - X1) - 1, [wall]);
hallway({X1, Y1}, {X2, Y2}) when (Y1 == Y2) and (X1 > X2) ->
    hallway({X2, Y1}, {X1, Y2});
hallway({X1, Y1}, {X2, Y2}) when (X1 == X2) and (Y2 > Y1) ->
    col(X1 - 1, Y1 + 1, (Y2 - Y1) - 1, [wall]) ++
    col(X1,     Y1 + 1, (Y2 - Y1) - 1, [walkable]) ++
    col(X1 + 1, Y1 + 1, (Y2 - Y1) - 1, [wall]);
hallway({X1, Y1}, {X2, Y2}) when (X1 == X2) and (Y1 > Y2) ->
    hallway({X1, Y2}, {X2, Y1});
hallway({X1, Y1}, {X2, Y2}) when (X1 /= X2) and (Y1 /= Y2) ->
    hallway({X1, Y1}, {X1, Y2}) ++
    hallway({X1, Y2}, {X2, Y2}) ++
    % this is just an elbow
    room(X1, Y2, 2, 2).
    

generate_test_world() ->
    Room1 = generate_random_room_with_random_door(0,0),
    Room2 = generate_random_room_with_random_door(15,0),
    Hallway = [],%%connect_rooms(Room1, Room2),
    Room1 ++ Room2 ++ Hallway.

connect_rooms(Room1, Room2) ->
    %for now we know that room2 is right of room1
    Room1_xyij = room_as_xyij(Room1),
    Room2_xyij = room_as_xyij(Room2),
    connect_room_xyijs(Room1_xyij, Room2_xyij).

connect_room_xyijs({{R1X, R1Y}, {R1I, R1J}},
                   {{R2X, R2Y}, {R2I, R2J}}) ->
    % if we overlap in both x and y, we need overlap logic
    % if we overlap in just x or just y, make a hallway
    %for now we know that room2 is right of room1
    RandomR1Coordinate = R1Y + random:uniform(R1J - 2),
    RandomR2Coordinate = R2Y + random:uniform(R2J - 2),
    hallway({R1X + R1I, RandomR1Coordinate},
            {R2X, RandomR2Coordinate}).
    


room_as_xyij(Room) ->
    FXs = fun(Elem) ->
        {X, _} = Elem#world.loc,
        X
    end,
    FYs = fun(Elem) ->
        {_, Y} = Elem#world.loc,
        Y
    end,
    MinX = lists:min(lists:map(FXs, Room)),
    MinY = lists:min(lists:map(FYs, Room)),
    MaxX = lists:max(lists:map(FXs, Room)),
    MaxY = lists:max(lists:map(FYs, Room)),
    {{MinX, MinY}, {MaxX - MinX, MaxY - MinY}}.
    
    

%work in progress
add_room_to_world(World) ->
    % generate some random coordinates
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    {MaxX, MaxY} = bounding_dimensions(World),
    X = random:uniform(MaxX), % todo check if these need to be -1
    Y = random:uniform(MaxY),
    generate_random_room(X, Y).

    


% x and y are the coordinates of the top left corner
% generates random coordinates for the bottom right
% bottom right will be at least x + 2, y + 2
generate_random_room(X, Y) ->
    I = random:uniform(10) + 2,
    J = random:uniform(10) + 2,
    room(X, Y, I, J).

%%%% random door stuff is probably totally unnecessary
generate_random_room_with_random_door(X, Y) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    I = random:uniform(10) + 2,
    J = random:uniform(10) + 2,
    room_with_door(X, Y, I, J, generate_random_door_coordinates(X, Y, I, J)).
   
generate_random_door_coordinates(X, Y, I, J) ->
    XY_bit = random:uniform(2) - 1, %% x or y wall
    NF_bit = random:uniform(2) - 1, %% near or far wall
    RandomXCoordinate = X + random:uniform(I - 2),
    RandomYCoordinate = Y + random:uniform(J - 2),
    case {XY_bit, NF_bit} of
        {0, 0} -> {RandomXCoordinate, Y};
        {0, 1} -> {RandomXCoordinate, Y + J - 1};
        {1, 0} -> {X, RandomYCoordinate};
        {1, 1} -> {X + I - 1, RandomYCoordinate}
    end.
