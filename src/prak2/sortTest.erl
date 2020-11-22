%%%-------------------------------------------------------------------
%%% @author Justin Hoffmann
%%% @author Hugo Protsch
%%%-------------------------------------------------------------------
-module(sortTest).
-author("Justin Hoffmann").
-author("Hugo Protsch").
-include_lib("eunit/include/eunit.hrl").

insertionS_test() ->
  runTests(fun sort:insertionS/1).

qsort_test() ->
  runTests(fun qsort/1).

hsort_test() ->
  runTests(fun sort:hsort/1).

runTests(Function) ->
  lists:foreach(
    fun({Sorted, UnsortedLists}) ->
      lists:foreach(
        fun(Unsorted) ->
          io:format("inner\r\n"),
          ?assertEqual(Sorted, Function(Unsorted))
        end, UnsortedLists)
    end, data()
  )
.


%%% List of Tupels with the following structure:
%%% { correctlySortedList, [nonSortedLs1, nonSortedLs2, nonSortedLs3, ...] }
data() ->
  [
    {
      [1, 2, 3, 4, 5, 6, 7],
      [
        [7, 6, 5, 4, 3, 2, 1],
        [2, 1, 3, 4, 5, 6, 7],
        [7, 6, 5, 4, 3, 2, 1]
      ]
    },
    {
      [1, 2, 3],
      [
        [1, 2, 3],
        [3, 2, 1],
        [2, 3, 1],
        [2, 1, 3]
      ]
    },
    {
      [1, 2],
      [
        [2, 1],
        [1, 2]
      ]
    },
    {[1], [[1]]},
    {[], [[]]}
  ].

qsortSwitchNumbers() -> [1, 2, 4].

%%% Runs qsort/3 on the List L with all piviot-methods
%%% and all switch-numbers defined in qsortSwitchNumbers.
%%% Returns the sorted list, if all results are the same,
%%% otherwise throws assert exception.
qsort(L) ->
  Results = lists:map(
    fun(N) ->
      R1 = sort:qsort(left, L, N),
      R2 = sort:qsort(middle, L, N),
      R3 = sort:qsort(right, L, N),
      R4 = sort:qsort(median, L, N),
      R5 = sort:qsort(random, L, N),
      ?assertEqual(R1, R2),
      ?assertEqual(R2, R3),
      ?assertEqual(R2, R4),
      ?assertEqual(R2, R5),
      R1
    end, qsortSwitchNumbers()),

  [H, T] = Results,
  lists:foreach(
    fun(N) ->
      ?assertEqual(N, H)
    end, T
  ),
  H
.

%%%% HELPER TESTS %%%%

insertToList_test() ->
  ?assertEqual([1], sort:insertToList([], 1)),
  ?assertEqual([1, 2, 3, 4], sort:insertToList([2, 3, 4], 1)),
  ?assertEqual([1, 2, 3, 4], sort:insertToList([1, 3, 4], 2)),
  ?assertEqual([1, 2, 3, 4], sort:insertToList([1, 2, 4], 3)),
  ?assertEqual([1, 2, 3, 4], sort:insertToList([1, 2, 3], 4)).
