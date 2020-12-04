qsort(_, [], _) -> [];
qsort(PivotMethod, List, SwitchNumber) ->
  {Pivot, ListWithoutPivot, Length} = getPivotAndLength(List, PivotMethod),
  if
    Length =< SwitchNumber -> insertionS(List);
    true ->
      {Left, Right} = getLeftRightFromPivot(ListWithoutPivot, Pivot),
      qsort(PivotMethod, Left, SwitchNumber) ++
        [Pivot] ++
        qsort(PivotMethod, Right, SwitchNumber)
  end
.

%% returns {numbersSmallerThanPivot, numbersLargerThanPivot}
getLeftRightFromPivot(List, Pivot) ->
  getLeftRightFromPivot(List, Pivot, [], []).

getLeftRightFromPivot([], _Pivot, L, R) -> {L, R};
getLeftRightFromPivot([H | T], Pivot, L, R) when H < Pivot ->
  getLeftRightFromPivot(T, Pivot, [H | L], R);
getLeftRightFromPivot([H | T], Pivot, L, R) ->
  getLeftRightFromPivot(T, Pivot, L, [H | R]).


%% returns {pivot, listWithoutPivot, length}
getPivotAndLength([], _) -> {none, [], 0};
getPivotAndLength([H | T], left) -> {H, T, listLength(T, 1)};
getPivotAndLength(L, right) ->
  Length = listLength(L, 0),
  {Pivot, Rest} = listGetNthAndRest(L, Length - 1, []),
  {Pivot, Rest, Length};
getPivotAndLength(L, middle) ->
  Length = listLength(L, 0),
  {Pivot, Rest} = listGetNthAndRest(L, Length div 2, []),
  {Pivot, Rest, Length};
getPivotAndLength(L, random) -> getRandomAndLength(L);
getPivotAndLength(L, median) -> listGetMedianAndLength(L).


getRandomAndLength(L) ->
  Length = listLength(L, 0),
  {Nth, Rest} = listGetNthAndRest(L, 0, []),
  {Nth, Rest, Length}.

listGetNth([H | _], 0) -> H;
listGetNth([_ | T], N) -> listGetNth(T, N - 1).

listGetNthAndRest([H | T], 0, Acc) -> {H, T ++ Acc};
listGetNthAndRest([H | T], N, Acc) -> listGetNthAndRest(T, N - 1, Acc ++ [H]).

listLength([], Cnt) -> Cnt;
listLength([_ | T], Cnt) -> listLength(T, Cnt + 1).

%% returns {medianElement, listWithoutMedian, length}
listGetMedianAndLength(L) ->
  [First | _] = L,
  {Last, _, Length} = getPivotAndLength(L, right),
  {Middle, _} = listFindFirstLargerThanAndRest(L, Length / 2),
  {Median, R} = listFindFirstLargerThanAndRest(L, (First + Last + Middle) div Length),
  {Median, R, Length}.


%% returns {firstValLargerThanVal, listWithoutVal}
listFindFirstLargerThanAndRest(L, Val) ->
  listFindFirstLargerThanAndRest(L, Val, []).
listFindFirstLargerThanAndRest([H | []], Val, Acc) -> {H, Acc};
listFindFirstLargerThanAndRest([H | T], Val, Acc) when H < Val ->
  listFindFirstLargerThanAndRest(T, Val, Acc ++ [H]);
listFindFirstLargerThanAndRest([H | T], Val, Acc) -> {H, Acc ++ T}.
