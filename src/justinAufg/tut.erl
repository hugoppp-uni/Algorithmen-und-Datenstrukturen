%% @author justinh
-module(tut).
-import(string, [len/1, concat/2, chr/2, substr/3,
                to_lower/1, to_upper/1]).

%% API functions
-export([main/0]).

%% Internal functions
main() -> 
    compare(4, 4.0).

compare(A, B) ->
    A == B.
    