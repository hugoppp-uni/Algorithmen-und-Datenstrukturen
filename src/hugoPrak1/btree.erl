%%%-------------------------------------------------------------------
%%% @author hugop
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2020 20:03
%%%-------------------------------------------------------------------
-module(btree).
-author("hugop").

%% API
-export([isEmptyBT/1, findBT/2, initBT/0, inOrderBT/1, insertBT/2]).

%% initBT: ∅ → btree
%% initBT()
initBT() -> {}.

%% isEmptyBT: btree → bool
%% isEmptyBT(<BTree>)
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%% findBT: btree × elem → integer
%% findBT(<BTree>,<Element>)
findBT({Element, H, _L, _R}, Element) -> H;
findBT({E, _H, L, _R}, Element) when Element < E -> findBT(L, Element);
findBT({E, _H, _L, R}, Element) when Element > E -> findBT(R, Element);
findBT(_, _) -> -1.


%% inOrderBT: btree → list
%% inOrderBT(<BTree>)
inOrderBT(T) -> inOrderBT(T, []).

inOrderBT({}, Acc) -> Acc;
inOrderBT({E, _, {}, {}}, Acc) -> [E | Acc];
inOrderBT({E, _, L, {}}, Acc) -> [inOrderBT(L, Acc) | E];
inOrderBT({_, _, {}, R}, Acc) -> Acc ++ inOrderBT(R);
inOrderBT({E, _, L, R}, Acc) -> inOrderBT(L, Acc) ++ [E] ++ inOrderBT(R).


%% isBT: btree → bool
%% isBT(<BTree>)

%% equalBT: btree × btree → bool
%% equalBT(<BTree>,<BTree>)

%% insertBT: btree × elem → btree
%% insertBT(<BTree>,<Element>)
insertBT({}, Element) -> {Element, 1, {}, {}};
insertBT({E, H, L, R}, Element) when Element < E ->
  {E, maxHeight(insertBT(L, Element), R)+ 1, insertBT(L, Element), R};
insertBT({E, H, L, R}, Element) when Element > E ->
  {E, maxHeight(L, insertBT(R, Element)) + 1, L, insertBT(R, Element)}.


%% deleteBT: btree × elem → btree
%% deleteBT(<BTree>,<Element>)


maxHeight({_, H1, _, _}, {_, H2, _, _})
  -> max(H1, H2);
maxHeight({}, {_E, H, _L, _R}) -> H;
maxHeight({_E, H, _L, _R}, {}) -> H;
maxHeight({}, {}) -> 0.


max(A, B) when A > B -> A;
max(_A, B) -> B.
