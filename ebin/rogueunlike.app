{application, rogueunlike,
 [{description, "entirely not exactly unlike a rogue"},
  {vsn, "0.0.3"},
  {modules, [
    %% main
    rogueunlike,

    %% cecho dependancies
    cecho, 
    cecho_srv, 

    %% modules
    rogueunlike_char,
    rogueunlike_input,
    rogueunlike_menu,
    rogueunlike_util,
    rogueunlike_world
    ]},
  {env, []},
  {mod, {rogueunlike, []}}]}.
