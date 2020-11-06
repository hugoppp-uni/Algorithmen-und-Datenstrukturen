%% @author Justin Hoffmann
-module(mylist).

%% API Functions
-export([main/0, member/2, substitute/4, compress/1, diffList/2, del_element/3,
        eo_count/1, isEven/1,  analyse/1]).

%% Internal functions

%% checks a given element for membership in a list
member(_, []) -> false;
member(Element, [Element|_]) -> true;
member(Element, [_|Tail]) -> member(Element, Tail).

%% substitutes an element from a list with a given element relative to a given position
substitute(_, _, [], _) -> [];
substitute(Element, Substitute, [Element|Tail], a) -> [Substitute|substitute(Element, Substitute, Tail, a)];
substitute(Element, Substitute, [Element|Tail], e) -> [Substitute|Tail];
substitute(Element, Substitute, List, l) -> invert(substitute(Element, Substitute, invert(List), e));
substitute(Element, Substitute, [Head|Tail], Position) -> [Head|substitute(Element, Substitute, Tail, Position)].

%% inverts a list
invert([]) -> [];
invert([Head|Tail]) -> invert(Tail) ++ [Head].

%% compresses a list to its unique elements and the number of their respective instances
compress([]) -> [];
compress([Head|Tail]) ->
    [{Head, count(Head, [Head|Tail])}] ++ compress(reduce(Head, Tail)).

%% counts all instances of an element in a given list
count(_, []) -> 0;
count(Element, [Element|Tail]) -> count(Element, Tail) + 1;
count(Element, [_|Tail]) -> count(Element, Tail).

%% reduce(E, L) via List Comprehension
reduce(Element, List) -> 
    [ListElement || ListElement <- List, ListElement =/= Element].

%% subtracts a list from another list
diffList(List1, List2) ->
    List1 -- List2.

%% removes an element from a list relative to a given position
del_element(_, _, []) -> [];
del_element(a, Element, [Element|Tail]) -> del_element(a, Element, Tail);
del_element(e, Element, [Element|Tail]) -> Tail;
del_element(l, Element, List) -> invert(del_element(e, Element, invert(List)));
del_element(Position, Element, [Head|Tail]) -> [Head|del_element(Position, Element, Tail)].

%% checks if a list has an even number of elements
isEven(List) -> isEven(List, true).
isEven([], Acc) -> Acc;
isEven([_|Tail], true) -> isEven(Tail, false);
isEven([_|Tail], false) -> isEven(Tail, true).

%% adds two tuple structures of size two together
add_Tuple({A, B}, {C, D}) ->
    {A+C, B+D}. 

%% counts the number of lists with an even and an odd amount of elements in a list
eo_count(List) -> eo_count(List, {1, 0}).
eo_count([], Acc) -> Acc;
eo_count([Head|Tail], {Even, Odd}) -> 
    case isEven(Tail) of
        true -> add_Tuple(eo_count(Head), eo_count(Tail, {Even-1, Odd+1}));
        false -> add_Tuple(eo_count(Head), eo_count(Tail, {Even+1, Odd-1}))
    end;
eo_count(_, _) -> {0, 0}.

%% main
main() ->
    true.
