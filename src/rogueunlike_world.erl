%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_world).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include_lib("stdlib/include/qlc.hrl").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([world_loop/0]).

%% ============================================================================
%% Module API
%% ============================================================================

world_loop() ->
    world_loop(nil).

world_loop(State) ->
    receive
        {init} -> 
            init_db(),
            world_loop(State);

        {database_test, _} ->
            create_test_world(),
            world_loop(State);

        {redraw, _} ->
            draw_world(),
            world_loop(State);

        {exit, _} ->
            ok;

        _ ->
            world_loop(State)
    end.


%% ============================================================================
%% Internal Functions
%% ============================================================================

test_db(Coord) ->
    Space = #world{loc = Coord, room = [wall]},
    Trans = fun() ->
        mnesia:write(Space)
    end,
    mnesia:transaction(Trans),
    console ! {msg, io_lib:format("Inserted space for ~p", [Coord])},
    DelTrans = fun() ->
        mnesia:delete({world, Coord})
    end,
    mnesia:transaction(DelTrans),
    console ! {msg, io_lib:format("Deleted space for ~p", [Coord])},
    ok.

draw_world() ->
    Q = qlc:q([X || X <- mnesia:table(world)]),
    F = fun() -> qlc:eval(Q) end,
    {atomic, World} = mnesia:transaction(F),
    {WorldWidth, WorldHeight} = bounding_dimensions(World),
    {DrawX, DrawY} = rogueunlike_util:centering_coords(WorldWidth, WorldHeight),
    DrawF = fun(Spot) ->
        Mob = Spot#world.mob,
        Stuff = Spot#world.stuff,
        Room = Spot#world.room,
        Char = square_char(Mob, Stuff, Room),
        {LocX, LocY} = Spot#world.loc,
        cecho:mvaddch(DrawY+LocY, DrawX+LocX, Char)
    end,
    lists:foreach(DrawF, World),
    cecho:refresh(),
    ok.

square_char(Mob, Stuff, Room) ->
    IsWall = proplists:get_bool(wall, Room),
    IsWalkable = proplists:get_bool(walkable, Room),
    
    case Mob of
        nil -> 
            case Stuff of
                [] -> 
                    case IsWall of
                        true -> $#;
                        _ -> 
                            case IsWalkable of
                                true -> $.;
                                _ -> $\s
                            end
                    end;
                [Thing] -> $T;
                _ -> $&
            end;
        MobChar ->
            $@
    end.

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
    % 3     #.....#
    % 4     ##..###
    % 5      #..#
    % 6      ####
    [#world{loc={0,0}, room=[wall]},
    #world{loc={1,0}, room=[wall]},
    #world{loc={2,0}, room=[wall]},
    #world{loc={3,0}, room=[wall]},
    #world{loc={4,0}, room=[wall]},
    #world{loc={5,0}, room=[wall]},
    #world{loc={6,0}, room=[wall]},
    #world{loc={7,0}, room=[wall]},
    #world{loc={0,1}, room=[wall]},
    #world{loc={1,1}, mob=hero, room=[walkable]},
    #world{loc={2,1}, room=[walkable]},
    #world{loc={3,1}, room=[walkable]},
    #world{loc={4,1}, room=[walkable]},
    #world{loc={5,1}, room=[walkable]},
    #world{loc={6,1}, room=[walkable]},
    #world{loc={7,1}, room=[wall]},
    #world{loc={8,1}, room=[wall]},
    #world{loc={9,1}, room=[wall]},
    #world{loc={0,2}, room=[wall]},
    #world{loc={1,2}, room=[wall]},
    #world{loc={2,2}, room=[wall]},
    #world{loc={3,2}, room=[wall]},
    #world{loc={4,2}, room=[walkable]},
    #world{loc={5,2}, room=[walkable]},
    #world{loc={6,2}, room=[walkable]},
    #world{loc={7,2}, room=[walkable]},
    #world{loc={8,2}, room=[walkable]},
    #world{loc={9,2}, room=[wall]},
    #world{loc={3,3}, room=[wall]},
    #world{loc={4,3}, room=[walkable]},
    #world{loc={5,3}, room=[walkable]},
    #world{loc={6,3}, room=[walkable]},
    #world{loc={7,3}, room=[walkable]},
    #world{loc={8,3}, room=[walkable]},
    #world{loc={9,3}, room=[wall]},
    #world{loc={3,4}, room=[wall]},
    #world{loc={4,4}, room=[wall]},
    #world{loc={5,4}, room=[walkable]},
    #world{loc={6,4}, room=[walkable]},
    #world{loc={7,4}, room=[wall]},
    #world{loc={8,4}, room=[wall]},
    #world{loc={9,4}, room=[wall]},
    #world{loc={4,5}, room=[wall]},
    #world{loc={5,5}, room=[walkable]},
    #world{loc={6,5}, room=[walkable]},
    #world{loc={7,5}, room=[wall]},
    #world{loc={4,6}, room=[wall]},
    #world{loc={5,6}, room=[wall]},
    #world{loc={6,6}, room=[wall]},
    #world{loc={7,6}, room=[wall]}].

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

