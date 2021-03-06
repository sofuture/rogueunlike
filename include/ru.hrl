
-define(ruMENU_CENTERED, 1).

-record(level, {id, data}).

-record(mob, {
        type = nil,
        ref = nil,
        func = nil,
        attackedby = nil}).

-record(world_state, {
        win = nil, 
        height = 0, 
        width = 0}).

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
        inventory = [],
        attributes = #cattributes{}}).

%% macros

-define(WINDOW_BORDERS, $|, $|, $-, $-, $+, $+, $+, $+).
-define(CONSOLE_BORDERS, $\s, $\s, $=, $\s, $=, $=, $\s, $\s).

-define(PP(X), io_lib:format("~p", [X])).
-define(MSG(X), ru_console:message(X)).

-define(ISDIR(A), A =:= kp_n orelse 
                  A =:= kp_s orelse 
                  A =:= kp_e orelse
                  A =:= kp_w orelse 
                  A =:= kp_nw orelse 
                  A =:= kp_ne orelse
                  A =:= kp_sw orelse 
                  A =:= kp_se orelse 
                  A =:= kp_center).

-define(CALL(A),
    gen_server:call(?MODULE, A)).

-define(CALL(A,B),
    gen_server:call(?MODULE, A, B)).
        
-define(CAST(A),
    gen_server:cast(?MODULE, A)).

