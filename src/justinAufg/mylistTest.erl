-module(mylistTest).
-author("Justin Hoffmann").
-import(mylist, [member/2, substitute/4, subAll/3, subFirst/3, subLast/3, invert/1, compress/1, count/2, reduce/2]).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
member_test() ->
substitute_test() ->
subAll_test() ->
subFirst_test() ->
subLast_test() ->
invert_test() ->
compress_test() ->
count_test() ->
reduce_test() ->