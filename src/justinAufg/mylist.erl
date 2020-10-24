%% @author Justin Hoffmann
-module(mylist).

%% API Functions
-export([member/2, substitute/4, compress/1]).

member(_, []) -> false;
member(Element, [Element|_]) -> true;
member(Element, [_|Tail]) -> member(Element, Tail).

%%

substitute(Element, Substitute, List, a) -> subAll(Element, Substitute, List);  
substitute(Element, Substitute, List, e) -> subFirst(Element, Substitute, List);
substitute(Element, Substitute, List, l) -> subLast(Element, Substitute, List). 

subAll(Element, Substitute, [Element|Tail]) -> [Substitute|subAll(Element, Substitute, Tail)];
subAll(Element, Substitute, [Head|Tail]) -> [Head|subAll(Element, Substitute, Tail)];
subAll(_, _, []) -> [].  

subFirst(Element, Substitute, [Element|Tail]) -> [Substitute|Tail];
subFirst(Element, Substitute, [_|Tail]) -> subFirst(Element, Substitute, Tail);
subFirst(_, _, []) -> [].

subLast(Element, Substitute, List) -> invert(subFirst(Element, Substitute, invert(List))).

invert([Head|Tail]) -> invert(Tail) ++ [Head];
invert([]) -> [].

%%

compress([Head|Tail]) ->
    [{Head, count(Head, [Head|Tail])}] ++ compress(reduce(Head, Tail));
compress([]) -> [].

count(_, []) -> 0;
count(Element, [Element|Tail]) -> count(Element, Tail) + 1;
count(Element, [_|Tail]) -> count(Element, Tail).

reduce(Element, [Element|Tail]) -> reduce(Element, Tail);
reduce(Element, [Head|Tail]) -> [Head] ++ reduce(Element, Tail);
reduce(_, []) -> [].























 

