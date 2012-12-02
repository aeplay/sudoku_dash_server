%%% =================================================================================== %%%
%%% Sudoku Dash: Game                                                                   %%%
%%%                                                                                     %%%
%%% Copyright 2012 Anselm Eickhoff                                                      %%%
%%%                                                                                     %%%
%%% This module manages one game of sudoku dash. Its job is to connect its history,     %%%
%%% which is holding its state, to the participating players. It does so by             %%%
%%% registering players as listeners of the history and offers actions that players     %%%
%%% can take, which will generate new history events.                                   %%%
%%% =================================================================================== %%%

-module(sdd_game).

%%% =================================================================================== %%%
%%% TESTS                                                                               %%%
%%% =================================================================================== %%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



-endif.