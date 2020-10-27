%%%-------------------------------------------------------------------
%%% @author hugop
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Okt 2020 11:26
%%%-------------------------------------------------------------------
-module(mylist).
-author("hugop").

%% API
-export([deleteLast/1, member/2, substitute/4, multisetAdd/2, compress/1]).


deleteLast([]) -> [];
deleteLast([_Last]) -> [];
deleteLast([Head | Tail]) ->
  [Head | deleteLast(Tail)].


%% member(E, L), überprüft, ob E Element der Liste L ist.
member(_M, []) -> false;
member(M, [H | _T]) when H == M -> true;
member(M, [_H | T]) -> member(M, T).


%%% substitute (E, S, L, P), ersetzt Element E in der Liste L durch S an der
%%% relativen Position P und gibt die neue Liste zurück. P bezeichnet die
%%% Position: e erstes, l letztes, a alle Vorkommen von E.
% reverse list => substitute first => reverse list
substitute(E, S, L, l) ->
  reverse(substitute(E, S, reverse(L), e, []));
substitute(E, S, L, P) -> substitute(E, S, L, P, []).

% when list is empty, return accumulator
substitute(_E, _S, [], _P, Acc) -> Acc;
% when element is not to be replaced, look at next element
substitute(E, S, [H | T], P, Acc) when E =/= H ->
  substitute(E, S, T, P, Acc ++ [H]);
% when element is to be replaced, replace element and decide what to do
substitute(E, S, [_H | T], P, Acc) ->
  if
  % first -> replace and return
    P == e -> Acc ++ [S | T];
  % all -> replace and rerun
    P == a -> substitute(E, S, T, P, Acc ++ [S])
  end.


%%% reverses List
reverse(L) -> reverse(L, []).

reverse([], Reversed) -> Reversed;
reverse([H | T], Reversed) -> reverse(T, [H | Reversed]).


compress(L) -> compress(L, []).

compress([H | []], M) -> multisetAdd(M, H);
compress([H | T], M) ->
  M2 = multisetAdd(M, H),
  compress(T, M2).


%%% Adds the element E to the multiset M
multisetAdd(M, E) -> multisetAdd(M, E, []).

% when element is currently inspected element in multiset, increase the count
% of the currently inspected element
multisetAdd([{E, MC} | T], E, Acc) -> Acc ++ [{E, MC + 1}] ++ T;
% when there is no elements in multiset, add an element with count 1
multisetAdd([], E, Acc) -> Acc ++ [{E, 1}];
% else, look at next element
multisetAdd([M | T], E, Acc) -> multisetAdd(T, E, Acc ++ [M]).



