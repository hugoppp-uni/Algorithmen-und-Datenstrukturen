%%%-------------------------------------------------------------------
%%% @author hugop
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Okt 2020 14:23
%%%-------------------------------------------------------------------
-module(mylistTest).
-author("hugop").
-import(mylist, [substitute/4, member/2, multisetAdd/2, compress/1]).

-include_lib("eunit/include/eunit.hrl").

%%simple_test() ->
%%  ?assert(true).

member_test() ->
  ?assert(member(a, [a, b, c, d])),
  ?assert(member(b, [a, b, c, d])),
  ?assert(member(d, [a, b, c, d])),
  ?assertNot(member(z, [a, b, c, d])),
  ?assertNot(member(z, [])).

substitute_emptyList_test() ->
  ?assertEqual([], substitute(a, x, [], e)),
  ?assertEqual([], substitute(a, x, [], a)),
  ?assertEqual([], substitute(a, x, [], l)).

substitute_e_test() ->
  ?assertEqual([x, z, z, a], substitute(a, x, [a, z, z, a], e)),
  ?assertEqual([z, x, z], substitute(a, x, [z, a, z], e)),
  ?assertEqual([z, z, x], substitute(a, x, [z, z, a], e)).

substitute_l_test() ->
  ?assertEqual([a, z, z, x], substitute(a, x, [a, z, z, a], l)),
  ?assertEqual([z, x, z], substitute(a, x, [z, a, z], l)),
  ?assertEqual([z, z, x], substitute(a, x, [z, z, a], l)).

substitute_a_test() ->
  ?assertEqual([x, z, z, x], substitute(a, x, [a, z, z, a], a)),
  ?assertEqual([z, z, z, z], substitute(a, x, [z, z, z, z], a)).

multiset_test() ->
  ?assertEqual([{e, 1}], multisetAdd([], e)),
  ?assertEqual([{a, 1}, {e, 1}], multisetAdd([{a, 1}], e)),
  ?assertEqual([{e, 2}], multisetAdd([{e, 1}], e)),
  ?assertEqual([{a, 3}, {e, 2}], multisetAdd([{a, 3}, {e, 1}], e)),
  ?assertEqual(
    [{a, 3}, {b, 2}, {c, 1}, {d, 1}], multisetAdd([{a, 3}, {b, 2}, {c, 1}], d)),
  ?assertEqual([{a, 2}, {b, 1}], multisetAdd([{a, 1}, {b, 1}], a)).


compress_test() ->
  ?assertEqual(
    [{a, 6}, {b, 3}, {c, 2}], compress([a, b, a, c, a, c, a, b, a, a, b])),
  ?assertEqual([{a, 1}, {b, 1}, {c, 1}], compress([a, b, c])).
