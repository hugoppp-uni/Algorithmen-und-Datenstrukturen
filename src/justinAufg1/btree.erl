%% @author Justin Hoffmann
-module(btree).
-author("Justin Hoffmann").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

%% API Functions
-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2,
  listAppend/2]).

%% Internal functions

%% initializes a btree
initBT() -> {}.

%% checks if a given btree is empty
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%% checks if a given btree is a valid btree
isBT(Btree) -> isBT(Btree, -1, ok).
isBT({}, _, _) -> true;
isBT({Element, Height, {}, {}}, LowerLimit, UpperLimit) ->
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height == 1);
isBT({Element, Height, Left, {}}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (HeightL + 1)) and
  (isBT(Left, LowerLimit, Element));
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and 
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (HeightR + 1)) and
  (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and 
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (maxInt(HeightL, HeightR) + 1)) and
  (isBT(Left, LowerLimit, Element)) and
  (isBT(Right, Element, UpperLimit)).

%% checks for semantic equality of 2 given btrees
equalBT(Btree1, Btree2) -> equalList(inOrderBT(Btree1), inOrderBT(Btree2)).

%% checks if two lists match
equalList([], []) -> true;
equalList(List, List) -> true;
equalList(_, _) -> false.


%% appends two lists
listAppend([Head | Tail], List) ->
  [Head | listAppend(Tail, List)];
listAppend([], List) ->
  List.

%% returns a list of all elements of a given btree in in_order
inOrderBT({}) -> [];
inOrderBT({Element, _, Left, Right}) ->
  listAppend(inOrderBT(Left), [Element | inOrderBT(Right)]).

%% inserts a given element into a given tree
insertBT({}, Element) ->
  {Element, 1, {}, {}};
insertBT({Element, Height, Left, Right}, Element) ->
  {Element, Height, Left, Right};
%% continue traversing left and count new height from bottom up
insertBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeft = insertBT(Left, Element),
  {NodeElement, max(getHeight(Right), getHeight(NewLeft)) + 1, NewLeft, Right};
%% continue traversing right and count new height from bottom up
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  NewRight = insertBT(Right, Element),
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}.

%% returns the greater of two integers
maxInt(Int1, Int2) when Int1 > Int2 -> Int1;
maxInt(Int1, Int2) when Int2 > Int1 -> Int2;
maxInt(Int, Int) -> Int.

%% returns the height of a given element in a given tree
findBT({Element, Height, _, _}, Element) -> Height;
findBT({NodeElement, _, Left, _}, Element) when Element < NodeElement ->
  findBT(Left, Element);
findBT({NodeElement, _, _, Right}, Element) when Element > NodeElement ->
  findBT(Right, Element);
findBT(_, _) -> -1. % doesn't exist

%% deletes a given element from a given tree
deleteBT({}, _) -> {};
deleteBT({Element, _, Left, Right}, Element) ->
  {Found, NewLeftTree} = findAndDeleteMax(Left),
  case Found == notfound of
    false ->
      {Found, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
    true -> Right
  end;
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeftTree = deleteBT(Left, Element),
  {NodeElement, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
deleteBT({NodeElement, _, Left, Right}, Element) ->
  NewRightTree = deleteBT(Right, Element),
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRightTree)) + 1, Left, NewRightTree}.

%% finds and deletes the biggest element in a given tree, returns element and new tree
findAndDeleteMax({}) -> {notfound, {}};
findAndDeleteMax({Found, _, _, {}}) -> {Found, {}};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
  {Found, NewRight} = findAndDeleteMax(Right),
  {Found, {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}}.

%% returns the height of a given tree
getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.