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

draw_world() ->
    Q = qlc:q([X || X <- mnesia:table(world)]),
    F = fun() -> qlc:eval(Q) end,
    {atomic, World} = mnesia:transaction(F),
    {WorldWidth, WorldHeight} = bounding_dimensions(World),
    {DrawX, DrawY} = rogueunlike_util:centering_coords(WorldWidth, WorldHeight),
    DrawF = fun(Spot) ->
        Char = square_char(Spot#world.stuff),
        {LocX, LocY} = Spot#world.loc,
        cecho:mvaddch(DrawY+LocY, DrawX+LocX, Char)
    end,
    lists:foreach(DrawF, World),
    cecho:refresh(),
    ok.

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
    [#world{loc={0,0}, stuff=[wall]},
    #world{loc={1,0}, stuff=[wall]},
    #world{loc={2,0}, stuff=[wall]},
    #world{loc={3,0}, stuff=[wall]},
    #world{loc={4,0}, stuff=[wall]},
    #world{loc={5,0}, stuff=[wall]},
    #world{loc={6,0}, stuff=[wall]},
    #world{loc={7,0}, stuff=[wall]},
    #world{loc={0,1}, stuff=[wall]},
    #world{loc={1,1}, stuff=[hero, walkable]},
    #world{loc={2,1}, stuff=[walkable]},
    #world{loc={3,1}, stuff=[walkable]},
    #world{loc={4,1}, stuff=[walkable]},
    #world{loc={5,1}, stuff=[walkable]},
    #world{loc={6,1}, stuff=[walkable]},
    #world{loc={7,1}, stuff=[wall]},
    #world{loc={8,1}, stuff=[wall]},
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
    #world{loc={4,3}, stuff=[walkable]},
    #world{loc={5,3}, stuff=[walkable]},
    #world{loc={6,3}, stuff=[walkable]},
    #world{loc={7,3}, stuff=[walkable]},
    #world{loc={8,3}, stuff=[walkable]},
    #world{loc={9,3}, stuff=[wall]},
    #world{loc={3,4}, stuff=[wall]},
    #world{loc={4,4}, stuff=[wall]},
    #world{loc={5,4}, stuff=[walkable]},
    #world{loc={6,4}, stuff=[walkable]},
    #world{loc={7,4}, stuff=[wall]},
    #world{loc={8,4}, stuff=[wall]},
    #world{loc={9,4}, stuff=[wall]},
    #world{loc={4,5}, stuff=[wall]},
    #world{loc={5,5}, stuff=[walkable]},
    #world{loc={6,5}, stuff=[walkable]},
    #world{loc={7,5}, stuff=[wall]},
    #world{loc={4,6}, stuff=[wall]},
    #world{loc={5,6}, stuff=[wall]},
    #world{loc={6,6}, stuff=[wall]},
    #world{loc={7,6}, stuff=[wall]}].

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
            {8, $U};
        walkable -> 
            {9, $.};
        wall ->
            {10, $#};
        _ ->
            {10000, $\s}
    end.



%% ============================================================================
%% Map Generation
%% ============================================================================
wall_across(X,Y,N) ->
    [
        #world{loc={I,Y}, stuff=[wall]} 
        || I <- lists:seq(X, X+N-1)
    ].
wall_down(X,Y,N) ->
    [
        #world{loc={X,J}, stuff=[wall]} 
        || J <- lists:seq(Y, Y+N-1)
    ].

row(X, Y, N, Type) ->
    [
        #world{loc={I,Y}, stuff=[Type]} 
        || I <- lists:seq(X, X+N-1)
    ].

col(X, Y, N, Type) ->
    [
        #world{loc={X,J}, stuff=[Type]} 
        || J <- lists:seq(Y, Y+N-1)
    ].

%% *<* dalexander has joined channel #lmit
%% <dalexander> grids?
grid(X, Y, I, J, Type) when X == I ->
    col(X, Y, J - Y + 1, Type);
grid(X, Y, I, J, Type) ->
    col(X, Y, J - Y + 1, Type) ++
    grid(X + 1, Y, I, J, Type).

room(X, Y, I, J) ->
    row(X, Y, I - X + 1, wall) ++ % top
    row(X, J, I - X + 1, wall) ++ % bottom
    col(X, Y + 1, J - Y - 1, wall) ++ % left
    col(I, Y + 1, J - Y - 1, wall) ++ % right
    grid(X + 1, Y + 1, I - 1, J - 1, walkable).
