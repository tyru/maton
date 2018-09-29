/*
 * This rules defines conversion between internal node and
 * ERE (Extended Regular Expressions) syntax.
 */
:- module(ereg, []).
:- consult(common).
:- use_module(common).

maton_rule.

toplevel(A) --> or(A).

or(or(A, B)) --> seq(A), tOr, or(B).
or(A) --> seq(A).

seq([]) --> [].
seq([Q|Qs]) --> quant(Q), seq(Qs).

quant(star(A)) --> group(A), tStar.
quant(plus(A)) --> group(A), tPlus.
quant(option(A)) --> group(A), tOption.
quant(repeat(A, N)) -->    % {n}
  group(A), tRepeatLeft, digits(N), tRepeatRight.
quant(repeat(A, 0, -1)) -->    % {,}
  group(A), tRepeatLeft, tRepeatMiddle, tRepeatRight.
quant(repeat(A, 0, M)) -->    % {,m}
  group(A), tRepeatLeft, tRepeatMiddle, digits(M), tRepeatRight.
quant(repeat(A, N, -1)) -->    % {n,}
  group(A), tRepeatLeft, digits(N), tRepeatMiddle, tRepeatRight.
quant(repeat(A, N, M)) -->    % {n,m}
  group(A), tRepeatLeft, digits(N), tRepeatMiddle, digits(M), tRepeatRight,
  {N =< M}.
quant(A) --> group(A).

% 1 or more digits
digits(N) --> when_parsing(parse_digits(0, N), generate_digits(N)).

generate_digits(N) --> [A], {atom_number(A, N)}.

parse_digits(C, N) --> digit(D), {C1 is C * 10 + D}, parse_digits(C1, N).
parse_digits(C, N) --> digit(D), {N is C * 10 + D}.
digit(D) --> [C], {atom_number(C, D)}.

group(group(A)) --> tGroupLeft, toplevel(A), tGroupRight.
group(A) --> charset(A).

charset(exclude(A)) --> tExcludeLeft, charset1(A), tExcludeRight.
charset(include(A)) --> tIncludeLeft, charset1(A), tIncludeRight.
charset(A) --> char(A).

charset1([]) --> [].
charset1([range(A, B) | Xs]) --> [A, '-', B], charset1(Xs).
charset1([A, '-']) --> [A, '-'].
charset1([A | Xs]) --> ['\\', C], {atom_concat('\\', C, A)}, charset1(Xs).
charset1([A | Xs]) --> [A], {not(charset_meta(A))}, charset1(Xs).
charset1([class(Class) | Xs]) --> class(Class), charset1(Xs).

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

char(dot) --> tDot.
char(bol) --> tBOL.
char(eol) --> tEOL.
char(A) --> ['\\', C], {esc(C, A)}.
char(char(A)) --> ['\\', C], {not(esc(C, _)), atom_concat('\\', C, A)}.
char(char(C)) --> [C], {not(meta(C))}.

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

tOr --> ['|'].
tStar --> ['*'].
tPlus --> ['+'].
tOption --> ['?'].
tRepeatLeft --> ['{'].
tRepeatMiddle --> [','].
tRepeatRight --> ['}'].
tGroupLeft --> ['('].
tGroupRight --> [')'].
tIncludeLeft --> ['['].
tIncludeRight --> [']'].
tExcludeLeft --> ['[', '^'].
tExcludeRight --> [']'].
tDot --> ['.'].
tBOL --> ['^'].
tEOL --> ['$'].
