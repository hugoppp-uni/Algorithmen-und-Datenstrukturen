%% @author Justin Hoffmann
%% @author Hugo Protsch
-module(btree).
-author("Justin Hoffmann").
-author("Hugo Protsch").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

%% API Functions
-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2,
  listAppend/2]).

%% Internal functions

%% initialisiert einen leeren Baum
initBT() -> {}.

%% überprüft einen Baum auf Leerheit
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%% überprüft einen Baum auf Validität
isBT(Btree) -> isBT(Btree, -1, ok).
% Der leere Baum selber ist ein valider BTree
isBT({}, _, _) -> true;
% Der Knoten ist ein Blatt, hat also 2 leere Kinder
isBT({Element, Height, {}, {}}, LowerLimit, UpperLimit) ->
% jedes Element ist eine Ganzzahl
  (is_integer(Element)) and
% jedes Element ist größer oder gleich 0
  (Element >= 0) and
% Prüfung durch dynamisches Intervall
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
% Höhe von allen Blättern im Baum ist laut Definition 1
  (Height == 1);
% Für den Fall, dass das rechte Kind leer ist, dies ist im Entwurf präteriert
isBT({Element, Height, Left, {}}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  (is_integer(Element)) and
  (Element >= 0) and
% Prüfen durch dynamisches Intervall
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
% negative Werte der Höhe nicht zulässig
  (Height > 0) and
% nur die linke Höhe ist relevant, da die rechte Höhe 0 ist
  (Height == (HeightL + 1)) and
% Setzen der neuen Intervallgrenzen
  (isBT(Left, LowerLimit, Element));
% Für den Fall, dass das linke Kind leer ist, dies ist im Entwurf präteriert
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and 
% Prüfen durch dynamisches Intervall
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height > 0) and
% nur die rechte Höhe ist relevant, da die linke Höhe 0 ist
  (Height == (HeightR + 1)) and
% Setzen der neuen Intervallgrenzen
  (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and
% Prüfen durch dynamisches Intervall
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height > 0) and
% Höhe wird nach Definition neu berechnet
  (Height == (maxInt(HeightL, HeightR) + 1)) and
% Setzen der neuen Intervallgrenzen
  (isBT(Left, LowerLimit, Element)) and
  (isBT(Right, Element, UpperLimit)).

%% überprüft zwei Bäume auf semantische Gleichheit
% inOrderBT wird für einen Listenvergleich benutzt, equalList prüft dann die Listen durch
% Pattern Matching
equalBT(Btree1, Btree2) -> equalList(inOrderBT(Btree1), inOrderBT(Btree2)).

%% überprüft, ob zwei Listen die gleich sind
% weniger syntaktischer Aufwand, daher im Entwurf präteriert
equalList([], []) -> true;
equalList(List, List) -> true;
equalList(_, _) -> false.

%% appendiert eine Liste an eine Andere
% weniger syntaktischer Aufwand, daher im Entwurf präteriert
listAppend([Head | Tail], List) ->
  [Head | listAppend(Tail, List)];
listAppend([], List) ->
  List.

%% gibt einen Baum in In-Order als Liste zurück
inOrderBT({}) -> [];
inOrderBT({Element, _, Left, Right}) ->
% der linke Teilbaum wird prependiert, der rechte Teilbaum appendiert und das Element 
% kommt in die Mitte
  listAppend(inOrderBT(Left), [Element | inOrderBT(Right)]).

%% insertiert ein gegebenes Element in einen gegebenen Baum
insertBT({}, Element) ->
% Element wird ganz unten in einen neuen Knoten mit der Höhe 1 eingefügt
  {Element, 1, {}, {}};
insertBT({Element, Height, Left, Right}, Element) ->
% Ist das Element bereits im Baum vorhanden, wird der Baum unverändert zurückgegeben
  {Element, Height, Left, Right};
% Traversierung nach links, da das Knoten-Element größer ist als das einzufügende Element
insertBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeft = insertBT(Left, Element),
% Bottom-Up-Berechnung der Höhe
  {NodeElement, max(getHeight(Right), getHeight(NewLeft)) + 1, NewLeft, Right};
% Traversierung nach rechts, da das Knoten-Element kleiner ist als das einzufügende Element
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  NewRight = insertBT(Right, Element),
% Bottom-Up-Berechnung der Höhe
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}.

%% gibt den größeren Wert zweier Ganzzahlen wieder
% weniger syntaktischer Aufwand, daher im Entwurf präteriert
maxInt(Int1, Int2) when Int1 > Int2 -> Int1;
maxInt(Int1, Int2) when Int2 > Int1 -> Int2;
maxInt(Int, Int) -> Int.

%% gibt die Höhe eines gegebenen Elements in einem gegebenen Baum wieder
% Element gefunden, Höhe zurückgeben
findBT({Element, Height, _, _}, Element) -> Height;
% Traversierung nach links, da das Knoten-Element größer ist als das einzufügende Element
findBT({NodeElement, _, Left, _}, Element) when Element < NodeElement ->
  findBT(Left, Element);
% Traversierung nach rechts, da das Knoten-Element kleiner ist als das einzufügende Element
findBT({NodeElement, _, _, Right}, Element) when Element > NodeElement ->
  findBT(Right, Element);
% Element nicht gefunden, -1 als Edge-Case ist im Entwurf präteriert
findBT(_, _) -> -1.

%% löscht ein gegebenes Element aus einem gegebenen Baum
deleteBT({}, _) -> {};
% ist rechts oder links leer und das zu löschende Element ist gefunden wird
% lediglich der rechte bzw. linke Teilbaum oben herangefügt
deleteBT({Element, _, {}, Right}, Element) -> Right;
deleteBT({Element, _, Left, {}}, Element) -> Left;
% ansonsten wird mit der Hilfsfunktion das Maximum aus dem linken Teilbaum herausgenommen
% und statt dem zu löschenden Element, in den aktuellen Knoten eingefügt
deleteBT({Element, _, Left, Right}, Element) ->
  {Found, NewLeftTree} = findAndDeleteMax(Left),
  {Found, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
% Traversierung nach links
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeftTree = deleteBT(Left, Element),
  {NodeElement, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
deleteBT({NodeElement, _, Left, Right}, Element) ->
% Traversierung nach rechts
  NewRightTree = deleteBT(Right, Element),
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRightTree)) + 1, Left, NewRightTree}.

%% findet und löscht das größte Element aus einem gegebenen Baum und gibt dieses und den neuen Baum zurück
% Maximum gefunden
findAndDeleteMax({Found, _, _, {}}) -> {Found, {}};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
% Traversierung nur nach rechts
  {Found, NewRight} = findAndDeleteMax(Right),
% Bottom-Up-Berechnung der Höhe
  {Found, {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}}.

%% gibt die Höhe eines Knotens wieder
% Erleichterung des Zugriffs auf die Höhe eines Knotens
% weniger syntaktischer Aufwand, daher im Entwurf präteriert
getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.