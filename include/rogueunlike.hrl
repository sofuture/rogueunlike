
-define(ruMENU_CENTERED, 1).

-record(level, {id, data}).

-record(console_state, {win = nil, lines = [], height = 0, width = 0}).

-record(cattributes, {
        strength = 0, 
        dexterity = 0, 
        constitution = 0,
        intelligence = 0,
        wisdom = 0,
        charisma = 0}).

-record(cstats, {
        name = "",
        gender = nil,
        race = nil,
        level = 0,
        gold = 0,
        hp = 0,
        hpmax = 0,
        ac = 0,
        turns = 0,
        cattributes = #cattributes{}}).

-define(CSTAT_LINE(Char),
        Name = Char#cstats.name,
        Race = Char#cstats.race,
        Level = Char#cstats.level,
        Hp = Char#cstats.hp,
        HpMax = Char#cstats.hpmax,
        Format = "~s the ~s (Lvl ~p) HP: ~p/~p",
        io_lib:format(Format, [Name, Race, Level, Hp, HpMax])).

-define(WINDOW_BORDERS, $|, $|, $-, $-, $+, $+, $+, $+).

-define(CONSOLE_BORDERS, $\s, $\s, $=, $\s, $=, $=, $\s, $\s).
