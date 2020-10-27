%% @author Justin Hoffmann
-module(mylist).

%% API Functions
-export([member/2, substitute/4, compress/1, diffList/2, del_element/3,
        eo_count/1]).

%% Internal functions

%% member(E, L) überprüft ob Element E in Liste L min. einmal vorkommt
member(_, []) -> false;
member(Element, [Element|_]) -> true;
member(Element, [_|Tail]) -> member(Element, Tail).

%% substitute(E, S, L, P) ersetzt Element E in Liste E mit dem Substitut S
%% relativ zur Position P
substitute(_, _, [], _) -> [];
substitute(Element, Substitute, [Element|Tail], a) -> [Substitute|substitute(Element, Substitute, Tail, a)];
substitute(Element, Substitute, [Element|Tail], e) -> [Substitute|Tail];
substitute(Element, Substitute, List, l) -> invert(substitute(Element, Substitute, invert(List), e));
substitute(Element, Substitute, [Head|Tail], Position) -> [Head|substitute(Element, Substitute, Tail, Position)].

%% invert(L) invertiert die Liste L
invert([]) -> [];
invert([Head|Tail]) -> invert(Tail) ++ [Head].


%% compress(L) gibt eine Liste aller einzigartigen Elemente der Liste L und deren
%% respektive Vorkommens-Zahl zurück
compress([]) -> [];
compress([Head|Tail]) ->
    [{Head, count(Head, [Head|Tail])}] ++ compress(reduce(Head, Tail)).

%% count(E, L) zählt die Vorkommnisse von Element E in Liste L
count(_, []) -> 0;
count(Element, [Element|Tail]) -> count(Element, Tail) + 1;
count(Element, [_|Tail]) -> count(Element, Tail).

%% reduce(E, L) via List Comprehension
reduce(Element, List) -> 
    [ListElement || ListElement <- List, ListElement =/= Element].



%% diffList(L1, L2) bildet die Listendifferenz aus L1 und L2
diffList(List1, List2) ->
    List1 -- List2.



%% del_element(P, E, L) entfernt Element E aus Liste L relativ zur Position P
del_element(_, _, []) -> [];
del_element(a, Element, [Element|Tail]) -> del_element(a, Element, Tail);
del_element(e, Element, [Element|Tail]) -> Tail;
del_element(l, Element, List) -> invert(del_element(e, Element, invert(List)));
del_element(Position, Element, [Head|Tail]) -> [Head|del_element(Position, Element, Tail)].




eo_count(List) -> 
    List = true.
    

























 

