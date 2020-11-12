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

-import(btree, [isEmptyBT/1, findBT/2, initBT/0]).
-include_lib("eunit/include/eunit.hrl").


isEmptyBT_test() ->
  ?assert(isEmptyBT({})),
  ?assertNot(isEmptyBT({5, 1, {}, {}})).

findBT_test() ->
  ?assertEqual(-1, findBT({}, 50)),
  ?assertEqual(1, findBT({50, 1, {}, {}}, 50)),
  ?assertEqual(2, findBT(correctTree2H(), 500)),
  ?assertEqual(1, findBT(correctTree2H(), 1250)),
  ?assertEqual(1, findBT(correctTree2H(), 2000)).


correctTree2H() ->
  {1000, 3,
            {500, 2,
                    {250, 1, {}, {}},
                    {750, 1, {}, {}}},
            {1500, 2,
                    {1250, 1, {}, {}},
                    {2000, 1, {}, {}}}
  }.
