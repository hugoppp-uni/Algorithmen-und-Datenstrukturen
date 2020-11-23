%%%-------------------------------------------------------------------
%%% @author Justin Hoffmann
%%% @author Hugo Protsch
%%%-------------------------------------------------------------------
-module(sortAnalyse).
-author("Justin Hoffmann").
-author("Hugo Protsch").

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").

test_test() ->
  ?assertEqual(0, analyzeAll(10000, 10000, 5, 20)).


allFuncList() -> [fun sort:insertionS/1, fun qsortLeft/1, fun qsortRight/1].
%%allFuncList() -> [fun qsortLeft/1, fun qsortRight/1, fun qsortMedian/1,
%%  fun qsortMiddle/1, fun qsortRandom/1, fun sort:insertionS/1, fun sort:hsort/1].

analyzeAll(ListStartLength, StepSize, StepCount, AvgOver) ->
  Elements = lists:seq(ListStartLength, ListStartLength + StepCount * StepSize,
    StepSize),
  Times = lists:map(
    fun(Func) ->
      Res = lists:map(
        fun(Length) ->
          analyze(Func, AvgOver, Length)
        end, Elements
      ),
    Res
    end, allFuncList()),
   [Elements | Times].



analyze(Function, Count, Length) ->
  TimeAll = lists:map(
    fun(L) ->
      {Time, _} = timer:tc(Function, [L]),
      Time
    end, randomLists(Count, Length, 9999999)),
  lists:sum(TimeAll) div Count
.

switchNumber() -> 0.

qsortLeft(L) ->
  sort:qsort(left, L, switchNumber()).

qsortRight(L) ->
  sort:qsort(right, L, switchNumber()).

qsortMedian(L) ->
  sort:qsort(median, L, switchNumber()).

qsortMiddle(L) ->
  sort:qsort(middle, L, switchNumber()).

qsortRandom(L) ->
  sort:qsort(random, L, switchNumber()).


randomLists(Count, Length, MaxInt) ->
  randomLists(Length, Count, MaxInt, []).

randomLists(0, _Length, _MaxInt, Acc) -> Acc;
randomLists(Count, Length, MaxInt, Acc) ->
  randomLists(Count - 1, Length, MaxInt, [randomList(Length, MaxInt) | Acc]).


randomList(Length, MaxInt) ->
  random:seed(erlang:now()),
  randomList(Length, MaxInt, []).

randomList(0, MaxInt, Acc) -> Acc;
randomList(Length, MaxInt, Acc) ->
  randomList(Length - 1, MaxInt, [random:uniform(MaxInt) | Acc]).