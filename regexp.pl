:- module(regexp, [toplevel/3, seq/3, or/3, quant/3, atom/3, group/3, char/3]).

toplevel(Seq) --> seq(Seq).

seq([]) --> [].
seq([X|Xs]) --> or(X), seq(Xs).

or(or([Q|Qs])) --> quant(Q), tOr, or(Qs).
or(Q) --> quant(Q).

quant(star(A)) --> atom(A), tStar.
quant(plus(A)) --> atom(A), tPlus.
quant(option(A)) --> atom(A), tOption.
quant(A) --> atom(A).

atom(group(G)) --> group(G).
atom(char(C)) --> char(C).

group(G) --> tGroupLeft, seq(G), tGroupRight.

char(dot) --> tDot.
char(C) --> ['\\', C].
char(C) --> [C].
