%%%-------------------------------------------------------------------
%%% @author hugop
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2020 20:26
%%%-------------------------------------------------------------------
-module(btreetest).
-author("hugop").

-import(btree, [isEmptyBT/1, findBT/2, initBT/0, inOrderBT/1, insertBT/2]).
-include_lib("eunit/include/eunit.hrl").


isEmptyBT_test() ->
  ?assert(isEmptyBT({})),
  ?assertNot(isEmptyBT({5, 1, {}, {}})).

findBT_test() ->
  ?assertEqual(-1, findBT({}, 50)),
  ?assertEqual(1, findBT({50, 1, {}, {}}, 50)),
  ?assertEqual(2, findBT(correctTree3H(), 500)),
  ?assertEqual(1, findBT(correctTree3H(), 1250)),
  ?assertEqual(1, findBT(correctTree3H(), 2000)).

inOrder_test() ->
  ?assertEqual([], inOrderBT({})),
  ?assertEqual([250 | [500 | [750 | [1000 | [1250 | [1500 | [2000]]]]]]]
    , inOrderBT(correctTree3H())).

insert_test() ->
  ?assertEqual(correctTree4H(), insertBT(correctTree3H(), 100)).

correctTree3H() ->
  {1000, 3,
    {500, 2,
      {250, 1, {}, {}},
      {750, 1, {}, {}}},
    {1500, 2,
      {1250, 1, {}, {}},
      {2000, 1, {}, {}}}
  }.

correctTree4H() ->
  {1000, 4,
    {500, 3,
      {250, 2,
        {100, 1, {},{}},
        {}},
      {750, 1, {}, {}}},
    {1500, 2,
      {1250, 1, {}, {}},
      {2000, 1, {}, {}}}
  }.
