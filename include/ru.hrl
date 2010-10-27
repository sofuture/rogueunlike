
-define(ruMENU_CENTERED, 1).

-record(level, {id, data}).

-record(console_state, {
        win = nil, 
        lines = [],
        statline = [],
        height = 0, 
        width = 0}).

-record(input, {
        flags = [],
        buffer = []}).

-record(world, {
        loc = nil,
        stuff = [],
        effects = []}).

-record(cattributes, {
        strength = 0, 
        dexterity = 0, 
        constitution = 0,
        intelligence = 0,
        wisdom = 0,
        charisma = 0}).

-record(cstats, {
        name = nil,
        gender = nil,
        race = nil,
        level = 0,
        gold = 0,
        hp = 0,
        hpmax = 0,
        ac = 0,
        turns = 0,
        attributes = #cattributes{}}).

-define(WINDOW_BORDERS, $|, $|, $-, $-, $+, $+, $+, $+).

-define(CONSOLE_BORDERS, $\s, $\s, $=, $\s, $=, $=, $\s, $\s).

-define(PP(X), io_lib:format("~p", [X])).
