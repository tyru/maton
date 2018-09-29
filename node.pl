/*
 * This rules defines conversion between internal node and
 * textual representation which can be recognizable as Prolog term.
 */
:- module(node, []).

maton_rule.

toplevel(A) --> or(A).

or(or(A, B)) --> tOrLeft, tSeqLeft, seq(A), tSeqRight, tComma, or(B), tOrRight.
or(A) --> tSeqLeft, seq(A), tSeqRight.

seq([]) --> [].
seq([A]) --> quant(A).
seq([A|As]) --> quant(A), comma(As), seq(As).

quant(star(A)) --> tStarLeft, group(A), tStarRight.
quant(plus(A)) --> tPlusLeft, group(A), tPlusRight.
quant(option(A)) --> tOptionLeft, group(A), tOptionRight.
quant(repeat(A, N)) -->    % {n}
  tRepeatLeft, group(A), tRepeatMiddle, digits(N), tRepeatRight.
quant(repeat(A, N, M)) -->    % {n,m}
  tRepeatLeft, group(A), tRepeatMiddle, digits(N), tRepeatMiddle, digits(M), tRepeatRight,
  {N =< M}.
quant(A) --> group(A).

% 1 or more digits
digits(N) --> {atom_number(A, N)}, [A].

group(group(A)) --> tGroupLeft, toplevel(A), tGroupRight.
group(A) --> charset(A).

charset(exclude(A)) --> tExcludeLeft, charset1(A), tExcludeRight.
charset(include(A)) --> tIncludeLeft, charset1(A), tIncludeRight.
charset(A) --> char(A).

charset1([]) --> [].
charset1([range(A, B) | Xs]) --> tRangeLeft, [A], tComma, [B], tRangeRight, comma(Xs), charset1(Xs).
charset1([A, '-']) --> [A, '-'].
charset1([A | Xs]) --> ['\\', C], {atom_concat('\\', C, A)}, comma(Xs), charset1(Xs).
charset1([A | Xs]) --> [A], {not(charset_meta(A))}, comma(Xs), charset1(Xs).
charset1([class(Class) | Xs]) --> class(Class), comma(Xs), charset1(Xs).

class(A) --> ['[', ':'], lowers(Cs), {atom_chars(A, Cs)}, [':', ']'].

% 1 or more a-z
lowers([A]) --> lower(A).
lowers([A | Xs]) --> lower(A), lowers(Xs).

% a-z
lower(A) --> [A], {atom_codes(A, [N]), between(97, 122, N)}.

charset_meta('\\').
% charset_meta('^').    % [^^] and [a^] is valid pattern
charset_meta('[').
charset_meta(']').

% Add comma if given list is non-empty
comma([]) --> [].
comma([_|_]) --> tComma.

char(dot) --> tDot.
char(bol) --> tBOL.
char(eol) --> tEOL.
char(A) --> [A], {esc(_, A)}.
char(char(A)) --> tCharLeft, ['\\', C], tCharRight, {not(esc(C, _)), atom_concat('\\', C, A)}.
char(char(C)) --> tCharLeft, [C], {not(meta(C))}, tCharRight.

esc('n', nl).
esc('e', esc).
esc('t', tab).
esc('r', cr).
esc('b', bs).

meta('(').
meta(')').
meta('[').
meta(']').
meta('{').
meta('}').
meta('*').
meta('+').
meta('?').
meta('.').
meta('^').
meta('$').
meta('\\').

tOrLeft --> ['o', 'r', '('].
tOrRight --> [')'].
tSeqLeft --> ['['].
tSeqRight --> [']'].
tStarLeft --> ['s', 't', 'a', 'r', '('].
tStarRight --> [')'].
tPlusLeft --> ['p', 'l', 'u', 's', '('].
tPlusRight --> [')'].
tOptionLeft --> ['o', 'p', 't', 'i', 'o', 'n', '('].
tOptionRight --> [')'].
tRepeatLeft --> ['r', 'e', 'p', 'e', 'a', 't', '('].
tRepeatMiddle --> [','].
tRepeatRight --> [')'].
tGroupLeft --> ['g', 'r', 'o', 'u', 'p', '('].
tGroupRight --> [')'].
tIncludeLeft --> ['i', 'n', 'c', 'l', 'u', 'd', 'e', '('].
tIncludeRight --> [')'].
tExcludeLeft --> ['e', 'x', 'c', 'l', 'u', 'd', 'e', '('].
tExcludeRight --> [')'].
tCharLeft --> ['c', 'h', 'a', 'r', '('].
tCharRight --> [')'].
tComma --> [','].
tDot --> ['d', 'o', 't'].
tBOL --> ['b', 'o', 'l'].
tEOL --> ['e', 'o', 'l'].
tEscNL --> ['n', 'l'].
