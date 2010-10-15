%% ============================================================================
%% Copyright 2010 Jeff Zellner
%%
%% This software is provided with absolutely no assurances, guarantees, 
%% promises or assertions whatsoever.
%%
%% Do what thou wilt shall be the whole of the law.
%% ============================================================================

-module(rogueunlike_level).

-author("Jeff Zellner <jeff.zellner@gmail.com>").

-include("cecho.hrl").
-include("rogueunlike.hrl").

-export([draw_level/1, load_level/1, level_height/1, level_width/1]).

%% ============================================================================
%% Application API
%% ============================================================================

draw_level(Level) ->
    LHeight = level_height(Level),
    LWidth = level_width(Level),
    {X,Y} = rogueunlike_util:centering_coords(LWidth, LHeight),
    print_level(X, Y, LWidth, LHeight, Level),
    ok.

load_level(LevelName) ->
    FileName = LevelName ++ ".dat",
    Path = filename:dirname(code:which(?MODULE)) ++ "/../priv/" ++ FileName,
    case file:consult(Path) of
    {ok, [{LvlId, LvlData} = _Level]} -> 
        {ok, #level{
                id=LvlId, 
                data=LvlData
            }};
    {error, Reason} -> 
        {error, Reason}
    end.

level_height(_Level = #level{data = LData}) ->
    length(binary:split(LData, <<$\n>>, [global])).

level_width(_Level = #level{data = LData}) ->
    Rows = binary:split(LData, <<$\n>>, [global]),
    MaxLen = fun(Elem, Max) ->
        Len = size(Elem),
        case Len > Max of
            true -> Len;
            false -> Max
        end
    end,
    lists:foldl(MaxLen, 0, Rows).

%% ============================================================================
%% Internal Functions
%% ============================================================================

print_level(X, Y, LWidth, LHeight, Level) ->
    Win = cecho:newwin(LHeight + 2, LWidth + 2, Y - 1, X - 1),
    cecho:wborder(Win, ?WINDOW_BORDERS),
    print_lines(1, Win, binary:split(Level#level.data, <<$\n>>, [global])),
    cecho:wrefresh(Win),
    ok.

print_lines(_,_,[]) -> ok;
print_lines(Y, Win, [Line|Rest] = _Lines) ->
    print_line(Y, Win, binary_to_list(Line)),
    print_lines(Y + 1, Win, Rest).

print_line(Y, Win, Line) ->
    cecho:wmove(Win, Y, 1),
    cecho:waddstr(Win, Line).

