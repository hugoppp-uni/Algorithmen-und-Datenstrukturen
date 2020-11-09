%% @author Justin Hoffmann
-module(btree).
-author("Justin Hoffmann").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

%% API Functions
-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2, listAppend/2]).

%% Internal functions

%% initializes a btree
initBT() -> {}.

%% checks if a given btree is empty
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%% checks if a given btree is a valid btree
isBT({}) -> true;
isBT({Element, Height, {}, {}}) ->
    (is_integer(Element)) and
    (Element>=0) and
    (Height==1);
isBT({Element, Height, Left, {}}) ->
    {ElementL, HeightL, _, _} = Left,
    (is_integer(Element)) and
    (Element>=0) and
    (Height>0) and  
    (Height==(HeightL+1)) and
    (Element>ElementL) and
    (isBT(Left));
isBT({Element, Height, {}, Right}) ->
    {ElementR, HeightR, _, _} = Right,
    (is_integer(Element)) and
    (Element>=0) and
    (Height>0) and  
    (Height==(HeightR+1)) and
    (Element>ElementR) and
    (isBT(Right));
isBT({Element, Height, Left, Right}) ->
    {ElementL, HeightL, _, _} = Left,
    {ElementR, HeightR, _, _} = Right,
    (is_integer(Element)) and
    (Element>=0) and
    (Height>0) and
    (Height==(maxInt(HeightL, HeightR)+1)) and
    (Element>ElementL) and
    (Element<ElementR) and
    (isBT(Left)) and
    (isBT(Right)).

%% checks for semantic equality of 2 given btrees
equalBT(Btree1, Btree2) -> inOrderBT(Btree1) =:= inOrderBT(Btree2).

%% appends two lists
listAppend([Head|Tail], List) ->
    [Head|listAppend(Tail, List)];
listAppend([], List) ->
    List.

%% returns a list of all elements of a given btree in in_order
inOrderBT({}) -> [];
inOrderBT({Element, _, Left, Right}) -> listAppend(inOrderBT(Left), [Element|inOrderBT(Right)]).

%% inserts a given element into a given tree
insertBT({}, Element) -> 
    {Element, 1, {}, {}};
insertBT({Element, Height, Left, Right}, Element) -> {Element, Height, Left, Right};
%% continue traversing left and count new height from bottom up
insertBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement -> 
    NewLeft = insertBT(Left, Element),
    {_, NewLeftHeight, _, _} = NewLeft,
    case isEmptyBT(Right) of
        false   -> {_, RightHeight, _, _} = Right;
        true    -> RightHeight = 0
    end,
    {NodeElement, max(RightHeight, NewLeftHeight)+1, NewLeft, Right};
%% continue traversing right and count new height from bottom up
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
    NewRight = insertBT(Right, Element),
    {_, NewRightHeight, _, _} = NewRight,
    case isEmptyBT(Left) of
        false   -> {_, LeftHeight, _, _} = Left;
        true    -> LeftHeight = 0
    end,
    {NodeElement, maxInt(LeftHeight, NewRightHeight)+1, Left, NewRight}.

%% deletes a given element from a given tree
deleteBT({NodeElement, Height, Left, Right}, Element) when Element < NodeElement ->
    NewLeft = deleteBT(Left, Element),
    {_, NewLeftHeight, _, _} = NewLeft,
    case isEmptyBT(Right) of
        false   -> {_, RightHeight, _, _} = Right;
        true    -> RightHeight = 0
    end,
    {NodeElement, max(RightHeight, NewLeftHeight)+1, NewLeft, Right};
deleteBT({NodeElement, Height, Left, Right}, Element) when Element > NodeElement ->
    NewRight = deleteBT(Right, Element),
    {_, NewRightHeight, _, _} = NewRight,
    case isEmptyBT(Left) of
        false   -> {_, LeftHeight, _, _} = Left;
        true    -> LeftHeight = 0
    end, 
    {NodeElement, maxInt(LeftHeight, NewRightHeight)+1, Left, NewRight}.

%% deletes the element and rearranges the tree
% suche im linke tree nach dem größten element
deleteAndRearrange({Element, Height, Left, Right}) -> btree.

%% returns the greater of two integers
maxInt(Int1, Int2) when Int1 > Int2 -> Int1;
maxInt(Int1, Int2) when Int2 > Int1 -> Int2;
maxInt(Int, Int) -> Int.

%% returns the height of a given element in a given tree
findBT({Element, Height, _, _}, Element) -> Height;
findBT({NodeElement, _, Left, _}, Element) when Element < NodeElement -> findBT(Left, Element);
findBT({NodeElement, _, _, Right}, Element) when Element > NodeElement -> findBT(Right, Element);
findBT(_, _) -> -1.