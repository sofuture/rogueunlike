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

draw_level(_Level) ->
    ok.

load_level(LevelName) ->
    FileName = LevelName ++ ".dat",
    Path = filename:dirname(code:which(?MODULE)) ++ "/../priv/" ++ FileName,
    case file:consult(Path) of
    {ok, [{LvlId, LvlData} = _Level]} -> 
        {ok, #level{id=LvlId, data=LvlData}};
    {error, Reason} -> 
        {error, Reason}
    end.

level_height(_Level = #level{data = LData}) ->
    length(binary:split(LData, <<"\n">>)).

level_width(_Level = #level{data = LData}) ->
    Rows = binary:split(LData, <<"\n">>),
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


