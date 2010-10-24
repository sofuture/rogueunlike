{application, rogueunlike,
 [{description, "entirely not exactly unlike a rogue"},
  {vsn, "0.0.3"},
  {modules, [
    %% main
    ru,

    %% cecho dependancies
    cecho, 
    cecho_srv, 

    %% modules
    ru_char,
    ru_input,
    ru_console,
    ru_util,
    ru_world
    ]},
  {env, []},
  {mod, {ru, []}}]}.
