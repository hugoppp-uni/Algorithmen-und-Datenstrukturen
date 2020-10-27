%% @author Justin Hoffmann
-module(mylist).

%% API Functions
-export([member/2, substitute/4, compress/1]).

%% Internal functions

%% member(E, L) überprüft ob Element E in Liste L min. einmal vorkommt
member(_, []) -> false;
member(Element, [Element|_]) -> true;
member(Element, [_|Tail]) -> member(Element, Tail).

%% substitute(E, S, L, P) ersetzt Element E in Liste E mit dem Substitut S
%% relativ zur Position P
substitute(Element, Substitute, List, a) -> subAll(Element, Substitute, List);  
substitute(Element, Substitute, List, e) -> subFirst(Element, Substitute, List);
substitute(Element, Substitute, List, l) -> subLast(Element, Substitute, List). 

%% subAll(E, S, L) ersetzt alle Elemente E in Liste E mit dem Substitut S
subAll(_, _, []) -> [];
subAll(Element, Substitute, [Element|Tail]) -> [Substitute|subAll(Element, Substitute, Tail)];
subAll(Element, Substitute, [Head|Tail]) -> [Head|subAll(Element, Substitute, Tail)].


%% subFirst(E, S, L) ersetzt das erste Element E in Liste E mit dem Substitut S
subFirst(_, _, []) -> [];
subFirst(Element, Substitute, [Element|Tail]) -> [Substitute|Tail];
subFirst(Element, Substitute, [_|Tail]) -> subFirst(Element, Substitute, Tail).


%% subsLast(E, S, L) ersetzt das letzte Element E in Liste E mit dem Substitut S
subLast(Element, Substitute, List) -> invert(subFirst(Element, Substitute, invert(List))).

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

%% =================================================================
%% reduce(E, L) entfernt alle Vorkommnisse von E aus der Liste L
%% reduce(Element, [Element|Tail]) -> reduce(Element, Tail);
%% reduce(Element, [Head|Tail]) -> [Head] ++ reduce(Element, Tail);
%% reduce(_, []) -> [].
%% =================================================================

%% reduce(E, L) via List Comprehension
reduce(Element, List) -> 
    [ListElement || ListElement <- List, ListElement =/= Element].


























 

