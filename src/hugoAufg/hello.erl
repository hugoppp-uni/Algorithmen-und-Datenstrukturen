-module(hello).
-author("hugop").

%% API
-export([hello_world/0]).
-import(mylist, [deleteLast/1, member/2, multisetAdd/2, substitute/4, compress/1]).

hello_world() ->

  _ = multisetAdd([{a, 1}, {b, 1}], a),
  _ = multisetAdd([{a, 1}, {b, 1}], a),
  _ = compress([a, b, a, c, a, c, a, b, a, a, b]),
  _ = compress([a, b, a, c, a, c, a, b, a, a, b]),

  _ = member(5, [a, b, c, d]),
  _ = deleteLast([a, b, c, d, e]),
  _ = deleteLast([test, 1, 2, 3]),
  _ = substitute(a, x, [a, z, z], e),
  _ = substitute(a, x, [z, a, z], e),
  _ = substitute(a, x, [z, z, a], e),

  _ = substitute(a, x, [a, z, z], a),
  _ = substitute(a, x, [a, a, a], a),

  _ = substitute(a, x, [a, a, a, a, a, a], a),

  _ = substitute(a, x, [a, a, a, a, a, a], l),

  _ = substitute(a, b, [], a),
  _ = 1.
