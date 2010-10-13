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
    rogueunlike_level,
    rogueunlike_menu
    ]},
  {env, []},
  {mod, {rogueunlike, []}}]}.
