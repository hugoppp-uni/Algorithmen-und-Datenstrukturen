%% @author Justin Hoffmann
-module(btree).
-author("Justin Hoffmann").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

%% API Functions
-export([initBT/0]).

%% Internal functions
initBT() ->   
    timestamp().