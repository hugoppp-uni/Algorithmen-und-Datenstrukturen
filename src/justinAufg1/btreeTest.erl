%% @author Justin Hoffmann
-module(btreeTest).
-author("Justin Hoffmann").
-import(btree, [initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1,
                deleteBT/2, listAppend/2]).
-include_lib("eunit/include/eunit.hrl").

all_test() ->
    initBT_test(),
    isEmptyBT_test(),
    isBT_test(),
    equalBT_test(),
    insertBT_test(),
    deleteBT_test(),
    findBT_test(),
    inOrderBT_test(),
    listAppend_test(),
    maxInt_test(),
    success.

initBT_test() ->
    ?assertEqual(initBT(), {}).

isEmptyBT_test() ->
    ?assert(isEmptyBT({})),
    ?assertNot(isEmptyBT({5, 1, {}, {}})).

%TODO
isBT_test() -> true.
equalBT_test() -> true.
insertBT_test() -> true.
deleteBT_test() -> true.
findBT_test() -> true.
inOrderBT_test() -> true.
listAppend_test() -> true.
maxInt_test() -> true.

