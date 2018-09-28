:- module(regexp, []).

export([
  (toplevel(Seq) --> seq(Seq)),

  (seq([]) --> []),
  (seq([X|Xs]) --> or(X), seq(Xs)),

  (or(or([Q|Qs])) --> quant(Q), tOr, or(Qs)),
  (or(Q) --> quant(Q)),

  (quant(star(A)) --> atom(A), tStar),
  (quant(plus(A)) --> atom(A), tPlus),
  (quant(option(A)) --> atom(A), tOption),
  (quant(A) --> atom(A)),

  (atom(group(G)) --> group(G)),
  (atom(char(C)) --> char(C)),

  (group(S) --> tGroupLeft, seq(S), tGroupRight),

  (char(dot) --> tDot),
  % var(C) checks current translation is in chars -> node translation),
  % so it does not translate `char(a)` to `['\\', a]`
  (char(C) --> {var(C)}, ['\\', C]),
  (char(C) --> [C])
]).
