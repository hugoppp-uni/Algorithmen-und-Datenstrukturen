%%%-------------------------------------------------------------------
%%% @author Justin Hoffmann
%%% @author Hugo Protsch
%%%-------------------------------------------------------------------
-module(sort).
-author("Justin Hoffmann").
-author("Hugo Protsch").

%% API
-export([insertionS/1, qsort/3, hsort/1, insertToList/2]).

% TODO: bisheriger Aufwand für Tests / Analyse aller Methoden: ~3h
% TODO: bisheriger Aufwand für diese Methode (Entwurf): 2.5h (Code): 2.5h
% NOTE: In the draft, all operations are made on a single list. In this
% implementation, the list is split into two lists. This would corresponds to
% splitting the list in the draft at the index n and does not affect the algorithm.
insertionS(L) -> insertionS(L, []).

% <N1> If original List length is 1, this will return a list with  element.
% <N3> If n == length - 2 (last element of the list still to be inserted), this
%      will insert the last element to the list <N2> and return the sorted list <N4>.
% ----
% <N2> If n < length - 2, this will insert the element and recall insertionS,
%     which is equal to <E1>
insertionS([H | T], Acc) -> insertionS(T, insertToList(Acc, H));
% <N1> If original List length is 0, this will return empty list.
insertionS([], Acc) -> Acc.

% TODO: Add referencing comments
insertToList([], E) -> [E];
insertToList([H | T], E) when H > E -> [E | [H | T]];
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
qsort(PivotMethod, List, SwitchNumber) -> List.

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
