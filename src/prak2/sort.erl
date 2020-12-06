%%%-------------------------------------------------------------------
%%% @author Justin Hoffmann
%%% @author Hugo Protsch
%%%-------------------------------------------------------------------
-module(sort).
-author("Justin Hoffmann").
-author("Hugo Protsch").

%% API
-export([insertionS/1, qsort/3, hsort/1, insertToList/2]).

% TODO: bisheriger Aufwand für Tests / Analyse aller Methoden: ~4h
% TODO: bisheriger Aufwand für diese Methode (Entwurf): 2.5h (Code): 3h


% NOTE: In the draft, all operations are made on a single list. In this
% implementation, the list is split into two lists. This would corresponds to
% splitting the list in the draft at <sortedSectionLength> and does not affect the
% algorithm.
insertionS(L) -> insertionS(L, []).

% When list is not yet completely sorted <N2>, insert the current element to the
% sorted list <N4> and look at the next element in the list <E1>
insertionS([H | T], Acc) -> insertionS(T, insertToList(Acc, H));
% When whole list is sorted <N2>, return it <N3>.
insertionS([], Acc) -> Acc.


% <N5> Returns list with element when list is empty or
% <N8> when at the end of the list. (See <N8><N9> below)
insertToList([], E) -> [E];
% <N7> When the element to insert is smaller or equal to the current element,
%      insert the element at the front.
insertToList([H | T], E) when H > E -> [E | [H | T]];
% <N8> <N9> When the element to insert is larger than the current element,
%           insert it to the tail. There is no need to differentiate between
%           the end of list and middle of the list, because insertToList()
%           called with an empty list will return a list with the element in it.
insertToList([H | T], E) -> [H | insertToList(T, E)].


%% die Zahlen sind in der Erlang-Liste [ ] gehalten und zu sortieren.
%% Der in der Vorlesung vorgestellte Algorithmus ist so auf die Verwendung
%% von Listen (statt array) zu transformieren, dass das Kernkonzept erhalten bleibt!
%% Die Begründung dazu ist im Entwurf aufzuführen.
%% Als Schnittstelle ist sort:qsort(<pivot-methode>,<Liste>,<switch-number>) vorgegeben.
%% Dabei kann <pivot-methode> die Werte left / middle / right / median / random annehmen.
%% Die Zahl <switch-number> entscheidet, ab welcher Länge Insertion Sort eingesetzt wird,
%% d.h. Teillisten, die kürzer als diese Zahl sind, werden dann mit
%% Insertion Sort sortiert und nicht mehr mit Quicksort.
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
  {Pivot, Rest} = listGetNthAndRest(L, Length - 1),
  {Pivot, Rest, Length};
getPivotAndLength(L, middle) ->
  Length = listLength(L, 0),
  {Pivot, Rest} = listGetNthAndRest(L, Length div 2),
  {Pivot, Rest, Length};
getPivotAndLength(L, random) -> getRandomAndLength(L);
getPivotAndLength(L, median) -> listGetMedianAndLength(L).


getRandomAndLength(L) ->
  Length = listLength(L, 0),
  {Nth, Rest} = listGetNthAndRest(L, 0),
  {Nth, Rest, Length}.


listGetNthAndRest([Nth | Rest], 0) -> {Nth, Rest};
listGetNthAndRest([H | T], N) ->
  {Nth, Rest} = listGetNthAndRest(T, N - 1),
  {Nth, [H | Rest]}.

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
listFindFirstLargerThanAndRest([H | []], _, Acc) -> {H, Acc};
listFindFirstLargerThanAndRest([H | T], Val, Acc) when H < Val ->
  listFindFirstLargerThanAndRest(T, Val, Acc ++ [H]);
listFindFirstLargerThanAndRest([H | T], _, Acc) -> {H, Acc ++ T}.

%% die Zahlen sind in der Erlang-Liste [ ] als Eingabe und Ausgabe gehalten.
%% Der in der Vorlesung vorgestellte Algorithmus ist auf die Verwendung
%% einer Datenstruktur (statt array) zu transformieren, so dass das Kernkonzept
%% erhalten bleibt (siehe z.B. diese Animation, Achtung: die Animation enthält
%% array-abhängige Vorgehensweisen, die nicht zum Kernkonzept des Algorithmus gehören!
%% Für das Kernkonzept verwenden Sie bitte diese Animation)!
%% Die Begründung ist im Entwurf aufzuführen. Da das Verfahren eine
%% Vorabkodierung des Pfades zum einfügen von Elementen an der nächsten freien
%% Position benötigt, wird eine Berechnung (mit konstantem Aufwand!!) dazu hier angeboten: sort.erl.

%% returns {{}, InputList, OutputList}, sorts a list using the Heap-Sort-Algorithm
hsort([]) -> [];
hsort(List) ->
  {Heap, Size} = buildMaxHeap(List),
  {_, _, OutputList} = hsort(Heap, List, [], Size),
  OutputList.

hsort({}, InputList, OutputList, _) -> {{}, InputList, OutputList};
hsort(Heap, InputList, OutputList, Size) ->
  {MaxElement, _, _, _} = Heap,
  RootReplacedWithLast = replaceRootWithLast(Heap, Size),
  ReducedSize = Size-1,
  NewMaxHeap = heapify(RootReplacedWithLast),
  hsort(NewMaxHeap, InputList, [MaxElement | OutputList], ReducedSize).

%% builds a Max-Heap from a list
buildMaxHeap(List) -> buildMaxHeap(List, {}, 0).

buildMaxHeap([], AccHeap, Size) -> {AccHeap, Size};
buildMaxHeap(InputList, AccHeap, Size) -> 
  [Head|Tail] = InputList,
  NewSize = Size + 1,
  ElementInserted = insertIntoHeap(AccHeap, Head, NewSize),
  buildMaxHeap(Tail, ElementInserted, NewSize).
  
%% inserts an element into a max heap
insertIntoHeap({}, InsertElement, Size) -> {InsertElement, {}, {}, Size};
insertIntoHeap(HostHeap, InsertElement, Size) ->
  Path = calcPath(Size),
  insertWithPath(HostHeap, InsertElement, Size, Path).

%% inserts an element into a max heap using a given path
insertWithPath({}, InsertElement, Size, _) -> {InsertElement, {}, {}, Size};
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [l|RemainingPath]) when NodeElement >= InsertElement ->
  {NodeElement, insertWithPath(Left, InsertElement, Size, RemainingPath), Right, Index};
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [l|RemainingPath]) when NodeElement < InsertElement ->
  {InsertElement, insertWithPath(Left, NodeElement, Size, RemainingPath), Right, Index};
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [r|RemainingPath]) when NodeElement >= InsertElement ->
  {NodeElement, Left, insertWithPath(Right, InsertElement, Size, RemainingPath), Index};
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [r|RemainingPath]) when NodeElement < InsertElement ->
  {InsertElement, Left, insertWithPath(Right, NodeElement, Size, RemainingPath), Index}.

%% puts last element at root position in a max heap
replaceRootWithLast({}, _) -> {};
replaceRootWithLast({_, {}, {}, _}, _) -> {}; 
replaceRootWithLast(Heap, Size) ->
  Path = calcPath(Size),
  LastElement = getLast(Heap, Path),
  ReducedHeap = removeLast(Heap, Path),
  {_, Left, Right, Index} = ReducedHeap, 
  {LastElement, Left, Right, Index}.

getLast({NodeElement, {}, {}, _}, []) -> NodeElement;
getLast({_, Left, _, _}, [l|RemainingPath]) -> getLast(Left, RemainingPath);
getLast({_, _, Right, _}, [r|RemainingPath]) -> getLast(Right, RemainingPath).

removeLast({_, {}, {}, _}, []) -> {}; 
removeLast({NodeElement, Left, Right, Index}, [l|RemainingPath]) -> {NodeElement, removeLast(Left, RemainingPath), Right, Index};
removeLast({NodeElement, Left, Right, Index}, [r|RemainingPath]) -> {NodeElement, Left, removeLast(Right, RemainingPath), Index}.

%% makes the top element of a max heap descend down to an appropriate position 
heapify({}) -> {};
heapify({NodeElement, {}, {}, Index}) -> {NodeElement, {}, {}, Index};
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index}) when NodeElement >= LeftElement -> 
  {NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index};
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index}) when NodeElement < LeftElement -> 
  {LeftElement, heapify({NodeElement, LeftL, RightL, IndexL}), {}, Index};
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) 
  when (NodeElement >= LeftElement) and (NodeElement >= RightElement) ->
    {NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index};
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) when LeftElement >= RightElement ->
  {LeftElement, heapify({NodeElement, LeftL, RightL, IndexL}), {RightElement, LeftR, RightR, IndexR}, Index};
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) when LeftElement < RightElement ->
  {RightElement, {LeftElement, LeftL, RightL, IndexL}, heapify({NodeElement, LeftR, RightR, IndexR}), Index}.

% Kodierung des Feldes: Nachfolger von Position i ist 2*i links und 2*i+1 rechts
% berechnet den Pfad zur ersten leeren Position
% l steht fuer links, r fuer rechts
% Beispiel: sort:calcPath(1). --> []
% 		sort:calcPath(2). --> [l]
% 		sort:calcPath(3). --> [r]
% 		sort:calcPath(4). --> [l,l]
% 		sort:calcPath(5). --> [l,r]
% 		sort:calcPath(6). --> [r,l] 
% 		sort:calcPath(7). --> [r,r] 
calcPath(Number) -> calcPath(Number,[]).
% aktuelle Position ist Wurzel
calcPath(1,Accu) -> Accu;
% aktuelle Position ist gerade
calcPath(Number,Accu) when Number rem 2 =:= 0 -> calcPath(Number div 2,[l|Accu]);
% aktuelle Position ist ungerade
calcPath(Number,Accu) when Number rem 2 =/= 0 -> calcPath((Number-1) div 2,[r|Accu]).