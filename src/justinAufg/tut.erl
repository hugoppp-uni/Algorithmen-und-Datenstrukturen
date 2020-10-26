%% @author justinh
-module(tut).
-import(string, [concat/2]).
%% API functions
-export([minimum/1, maximum/1, minimum_maximum/1]).

%% Internal functions

%% minimum(L) ermittelt das kleinste Element der Liste L
minimum([Head|Tail]) -> minimum(Head, [Head|Tail]).
minimum(Element, [Head|Tail]) when Head < Element -> minimum(Head, Tail);
minimum(Element, [_|Tail]) -> minimum(Element, Tail);
minimum(Element, []) -> Element.

%% maximum(L) ermittelt das kleinste Element der Liste L
maximum([Head|Tail]) -> maximum(Head, [Head|Tail]).
maximum(Element, [Head|Tail]) when Head > Element -> maximum(Head, Tail);
maximum(Element, [_|Tail]) -> maximum(Element, Tail);
maximum(Element, []) -> Element.

%% minimum_maximum(L) gibt ein Tupel mit Maximum und Minimum der Liste L aus
minimum_maximum(List) -> {minimum(List), maximum(List)}.




