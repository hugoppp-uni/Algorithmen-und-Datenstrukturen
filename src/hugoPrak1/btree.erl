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
-export([isEmptyBT/1, findBT/2, initBT/0]).

%% initBT: ∅ → btree
%% initBT()
initBT() -> {}.

%% isEmptyBT: btree → bool
%% isEmptyBT(<BTree>)
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

%% findBT: btree × elem → integer
%% findBT(<BTree>,<Element>)
findBT({EToFind, H, _L, _R}, EToFind) -> H;
findBT({E, _H, L, _R}, EToFind) when EToFind < E -> findBT(L, EToFind);
findBT({E, _H, _L, R}, EToFind) when EToFind > E -> findBT(R, EToFind);
findBT(_, _) -> -1.