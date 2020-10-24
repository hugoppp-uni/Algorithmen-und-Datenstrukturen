%% @author Justin Hoffmann
-module(mylist).

%% API Functions
-export([member/2, substitute/4, invert/1]).

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

compress(List) -> 



















 

