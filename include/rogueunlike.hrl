
-define(ruMENU_CENTERED, 1).

-record(level, {id, data}).

-record(console_state, {win = nil, lines = [], height = 0, width = 0}).

-define(WINDOW_BORDERS, $|, $|, $-, $-, $+, $+, $+, $+).

-define(CONSOLE_BORDERS, $\s, $\s, $=, $\s, $=, $=, $\s, $\s).
