%%%-------------------------------------------------------------------
%%% @author Justin Hoffmann
%%% @author Hugo Protsch
%%%-------------------------------------------------------------------
-module(sort).
-author("Justin Hoffmann").
-author("Hugo Protsch").

%% API
-export([insertionS/1, qsort/3, hsort/1, insertToList/2]).

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


%% <pivot-methode>: left / middle / right / median / random
%% <switch-number>: unter dieser Länge wird Insertion Sort benutzt
qsort(_, [], _) -> [];
qsort(PivotMethod, List, SwitchNumber) ->
  % <N1>
  {Pivot, ListWithoutPivot, Length} = getPivotAndLength(List, PivotMethod),
  if
    % use insertionS when length < SwitchNumber
    Length =< SwitchNumber -> insertionS(List);
    % <N4>
    true ->
      % <N2>
      {Left, Right} = getLeftRightFromPivot(ListWithoutPivot, Pivot),
      % <N3a>
      qsort(PivotMethod, Left, SwitchNumber) ++
        [Pivot] ++
        % <N3b>
        qsort(PivotMethod, Right, SwitchNumber)
  end
.

%% returns {numbersSmallerThanPivot, numbersLargerThanPivot}
getLeftRightFromPivot(List, Pivot) ->
  getLeftRightFromPivot(List, Pivot, [], []).

% <N8> return L and R, when list is empty <N7>
getLeftRightFromPivot([], _Pivot, L, R) -> {L, R};
% <N6> Add to L and recall <E1>
getLeftRightFromPivot([H | T], Pivot, L, R) when H < Pivot ->
  getLeftRightFromPivot(T, Pivot, [H | L], R);
% <N5> Add to R and recall <E1>
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
  % <N1> build max-heap from inputlist
  {Heap, Size} = buildMaxHeap(List),
  % <N2> get sorted outputlist from hsort/4
  {_, _, OutputList} = hsort(Heap, List, [], Size),
  OutputList.

% <N3> heap empty? -> true
hsort({}, InputList, OutputList, _) -> {{}, InputList, OutputList};
% <N3> heap empty? -> false
hsort(Heap, InputList, OutputList, Size) ->
  % <N4> take root element from max-heap
  {MaxElement, _, _, _} = Heap,
  % <N5> <N6> swap root element with last element of heap,
  %           also delete root element now last element of heap
  RootReplacedWithLast = replaceRootWithLast(Heap, Size),
  ReducedSize = Size-1,
  % <N7> heapify and continue with reduced max-heap 
  NewMaxHeap = heapify(RootReplacedWithLast),
  % <N8> prepend max-element to outputlist
  hsort(NewMaxHeap, InputList, [MaxElement | OutputList], ReducedSize).

%% builds a Max-Heap from a list
buildMaxHeap(List) -> buildMaxHeap(List, {}, 0).

% <N1> list empty -> true
buildMaxHeap([], AccHeap, Size) -> {AccHeap, Size};
% <N1> list empty -> false
buildMaxHeap(InputList, AccHeap, Size) -> 
  % <N3> split List into Head and Tail
  [Head|Tail] = InputList,
  % <N2> increment size of heap
  NewSize = Size + 1,
  ElementInserted = insertIntoHeap(AccHeap, Head, NewSize),
  % continue with tail
  buildMaxHeap(Tail, ElementInserted, NewSize).
  
%% inserts an element into a max heap
% <N4> Heap Empty? -> true
insertIntoHeap({}, InsertElement, Size) -> {InsertElement, {}, {}, Size};
% <N4> Heap Empty? -> false
insertIntoHeap(HostHeap, InsertElement, Size) ->
  % <N5> calculate Path for current Position into path list
  Path = calcPath(Size),
  insertWithPath(HostHeap, InsertElement, Size, Path).

%% inserts an element into a max heap using a given path
% <N6> path list empty -> continue with tail
insertWithPath({}, InsertElement, Size, _) -> {InsertElement, {}, {}, Size};
% <N7> traverse according to path
% <N5> Temp > current Node? -> false
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [l|RemainingPath]) when NodeElement >= InsertElement ->
  {NodeElement, insertWithPath(Left, InsertElement, Size, RemainingPath), Right, Index};
% <N5> Temp > current Node? -> true  
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [l|RemainingPath]) when NodeElement < InsertElement ->
  {InsertElement, insertWithPath(Left, NodeElement, Size, RemainingPath), Right, Index};
% <N5> Temp > current Node? -> false
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [r|RemainingPath]) when NodeElement >= InsertElement ->
  {NodeElement, Left, insertWithPath(Right, InsertElement, Size, RemainingPath), Index};
% <N5> Temp > current Node? -> true
insertWithPath({NodeElement, Left, Right, Index}, InsertElement, Size, [r|RemainingPath]) when NodeElement < InsertElement ->
  {InsertElement, Left, insertWithPath(Right, NodeElement, Size, RemainingPath), Index}.

%% puts last element at root position in a max heap
% <N5> <N6> swap root with last element and remove last
replaceRootWithLast({}, _) -> {};
replaceRootWithLast({_, {}, {}, _}, _) -> {}; 
replaceRootWithLast(Heap, Size) ->
  Path = calcPath(Size),
  LastElement = getLast(Heap, Path),
  ReducedHeap = removeLast(Heap, Path),
  {_, Left, Right, Index} = ReducedHeap, 
  {LastElement, Left, Right, Index}.

%% returns last element from max-heap
getLast({NodeElement, {}, {}, _}, []) -> NodeElement;
getLast({_, Left, _, _}, [l|RemainingPath]) -> getLast(Left, RemainingPath);
getLast({_, _, Right, _}, [r|RemainingPath]) -> getLast(Right, RemainingPath).

%% removes the last element of a max-heap
% <N6> remove former root (now last element) from max-heap
removeLast({_, {}, {}, _}, []) -> {}; 
removeLast({NodeElement, Left, Right, Index}, [l|RemainingPath]) -> {NodeElement, removeLast(Left, RemainingPath), Right, Index};
removeLast({NodeElement, Left, Right, Index}, [r|RemainingPath]) -> {NodeElement, Left, removeLast(Right, RemainingPath), Index}.

%% makes the top element of a max heap descend down to an appropriate position 
heapify({}) -> {};
% <N1> both children empty? -> true
heapify({NodeElement, {}, {}, Index}) -> {NodeElement, {}, {}, Index};
% <N1> both children empty? -> false
% <N2> only right child empty? -> true
% <N3> tmp (here NodeElement) >= left child -> true
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index}) when NodeElement >= LeftElement -> 
  % <N6> return max-heap with tmp at current position
  {NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index};
% <N3> tmp (here NodeElement) >= left child -> false
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {}, Index}) when NodeElement < LeftElement ->
  % <N4> swap position of tmp and left child element and continue with new position of tmp (NodeElement)
  {LeftElement, heapify({NodeElement, LeftL, RightL, IndexL}), {}, Index};
% <N2> only right child empty? -> false
% <N5> tmp >= left child element AND tmp >= right child element -> true
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) 
  when (NodeElement >= LeftElement) and (NodeElement >= RightElement) ->
    % <N6> return max-heap with tmp at current position
    {NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index};
% <N5> tmp >= left child element AND tmp >= right child element -> false
% <N7> left child element >= right child element? -> true
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) when LeftElement >= RightElement ->
  % <N4> swap position with left child element and continue with new position of tmp (NodeElement)
  {LeftElement, heapify({NodeElement, LeftL, RightL, IndexL}), {RightElement, LeftR, RightR, IndexR}, Index};
% <N7> left child element >= right child element? -> false
heapify({NodeElement, {LeftElement, LeftL, RightL, IndexL}, {RightElement, LeftR, RightR, IndexR}, Index}) when LeftElement < RightElement ->
  % <N8> swap position with right child element and continue with new position of tmp (NodeElement)
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