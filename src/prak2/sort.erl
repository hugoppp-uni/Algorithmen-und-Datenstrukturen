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
% splitting the list in the draft at the index n and does not affect the algorithm.
insertionS(L) -> insertionS(L, []).

% <N2> Inserts nth element into the sorted List
% <E1> Recalls insertionS with n = n + 1 (Element to insert = next element)
insertionS([H | T], Acc) -> insertionS(T, insertToList(Acc, H));
% <N1> If original List length is 0, this will return an empty list.
% <N4> If the original List length is greater than 1, this will return the
% sorted List, once n == length (the last element has already been inserted.
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
      qsort(PivotMethod, [El || El <- ListWithoutPivot, El < Pivot], SwitchNumber) ++
        [Pivot] ++
        qsort(PivotMethod, [El || El <- ListWithoutPivot, El >= Pivot], SwitchNumber)
  end
.


%% returns {pivot, <list without pivot>, <length>}
getPivotAndLength([], _) -> {none, [], 0};
getPivotAndLength([H | T], left) -> {H, T, listLength(T, 1)};
getPivotAndLength(L, right) -> removeLastFromListAndGetLength(L, [], 0);
getPivotAndLength(L, middle) -> notImplemented; %TODO
getPivotAndLength(L, random) -> notImplemented; %TODO
getPivotAndLength(L, median) -> notImplemented. %TODO


%% returns {<last element>, <List without last>, <length>}
removeLastFromListAndGetLength([H | []], Acc, Cnt) -> {H, Acc, Cnt + 1};
removeLastFromListAndGetLength([H | T], Acc, Cnt) ->
  removeLastFromListAndGetLength(T, Acc ++ [H], Cnt + 1).


listLength([], Cnt) -> Cnt;
listLength([_ | T], Cnt) -> listLength(T, Cnt + 1).

%% die Zahlen sind in der Erlang-Liste [ ] als Eingabe und Ausgabe gehalten.
%% Der in der Vorlesung vorgestellte Algorithmus ist auf die Verwendung
%% einer Datenstruktur (statt array) zu transformieren, so dass das Kernkonzept
%% erhalten bleibt (siehe z.B. diese Animation, Achtung: die Animation enthält
%% array-abhängige Vorgehensweisen, die nicht zum Kernkonzept des Algorithmus gehören!
%% Für das Kernkonzept verwenden Sie bitte diese Animation)!
%% Die Begründung ist im Entwurf aufzuführen. Da das Verfahren eine
%% Vorabkodierung des Pfades zum einfügen von Elementen an der nächsten freien
%% Position benötigt, wird eine Berechnung (mit konstantem Aufwand!!) dazu hier angeboten: sort.erl.
hsort(List) -> List.
