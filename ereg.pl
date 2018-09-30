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
quant(repeat(A, nil, nil)) -->    % {,}
  group(A), tRepeatLeft, tRepeatMiddle, tRepeatRight.
quant(repeat(A, nil, M)) -->    % {,m}
  group(A), tRepeatLeft, tRepeatMiddle, digits(M), tRepeatRight.
quant(repeat(A, N, nil)) -->    % {n,}
  group(A), tRepeatLeft, digits(N), tRepeatMiddle, tRepeatRight.
quant(repeat(A, N, M)) -->    % {n,m}
  group(A), tRepeatLeft, digits(N), tRepeatMiddle, digits(M), tRepeatRight,
  {N =< M}.
quant(zero_match(A)) -->    % (?=abc)
  tZeroMatchLeft, toplevel(A), tZeroMatchRight.
quant(zero_non_match(A)) -->    % (?!abc)
  tZeroNonMatchLeft, toplevel(A), tZeroNonMatchRight.
quant(zero_pred_match(A)) -->    % (?<=abc)
  tZeroPredMatchLeft, toplevel(A), tZeroPredMatchRight.
quant(zero_pred_non_match(A)) -->    % (?<!abc)
  tZeroPredNonMatchLeft, toplevel(A), tZeroPredNonMatchRight.
quant(zero_no_retry_match(A)) -->    % (?>abc)
  tZeroNoRetryMatchLeft, toplevel(A), tZeroNoRetryMatchRight.
quant(A) --> group(A).

% 1 or more digits
digits(N) --> when_parsing(parse_digits(0, N), generate_digits(N)).

generate_digits(N) --> [A], {atom_number(A, N)}.

parse_digits(C, N) --> digit(D), {C1 is C * 10 + D}, parse_digits(C1, N).
parse_digits(C, N) --> digit(D), {N is C * 10 + D}.
digit(D) --> [C], {atom_number(C, D)}.

group(capture(A)) --> tCaptureLeft, toplevel(A), tCaptureRight.
group(group(A)) --> tGroupLeft, toplevel(A), tGroupRight.
group(A) --> charset(A).

charset(exclude(A)) --> tExcludeLeft, charset_exclude(A), tExcludeRight.
charset(include(A)) --> tIncludeLeft, charset_include(A), tIncludeRight.
charset(A) --> char(A).

charset_include([]) --> [].
charset_include([First | Cs]) --> charset_first(First), charset_exclude(Cs).

charset_first(char('^')) --> ['\\', '^'].
charset_first(A) --> charset_exclude([A]), {A \= char('^')}.

charset_exclude([]) --> [].
charset_exclude([range(A, B) | Xs]) --> [A, '-', B], charset_exclude(Xs).
charset_exclude([char(C), char('-')]) --> [C, '-'].
charset_exclude([A | Xs]) --> ['\\', C], {esc(C, A)}, charset_exclude(Xs).
charset_exclude([char(C) | Xs]) --> ['\\', C], {not(esc(C, _)), charset_meta(C)}, charset_exclude(Xs).
charset_exclude([char(C) | Xs]) --> [C], {not(charset_meta(C))}, charset_exclude(Xs).
charset_exclude([class(Class) | Xs]) --> class(Class), charset_exclude(Xs).

class(A) --> ['[', ':'], lowers(A), [':', ']'].

% 1 or more a-z
lowers(A) --> when_parsing(parse_lowers(A), generate_lowers(A)).

generate_lowers(A) --> chars(A).

parse_lowers(A) --> lowers_chars(Cs), {atom_chars(A, Cs)}.
lowers_chars([A]) --> lower(A).
lowers_chars([A | Xs]) --> lower(A), lowers_chars(Xs).

% a-z
lower(A) --> [A], {between(97, 122, N), atom_codes(A, [N])}.

charset_meta('\\').
% charset_meta('^').    % [^^] and [a^] is valid pattern
charset_meta('[').
charset_meta(']').

char(dot) --> tDot.
char(bol) --> tBOL.
char(eol) --> tEOL.
char(A) --> ['\\', C], {esc(C, A)}.
char(char(C)) --> ['\\', C], {not(esc(C, _)), meta(C)}.
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
tZeroMatchLeft --> chars('(?=').
tZeroMatchRight --> [')'].
tZeroNonMatchLeft --> chars('(?!').
tZeroNonMatchRight --> [')'].
tZeroPredMatchLeft --> chars('(?<=').
tZeroPredMatchRight --> [')'].
tZeroPredNonMatchLeft --> chars('(?<!').
tZeroPredNonMatchRight --> [')'].
tZeroNoRetryMatchLeft --> chars('(?>').
tZeroNoRetryMatchRight --> [')'].
tCaptureLeft --> ['('].
tCaptureRight --> [')'].
tGroupLeft --> chars('(?:').
tGroupRight --> [')'].
tIncludeLeft --> ['['].
tIncludeRight --> [']'].
tExcludeLeft --> chars('[^').
tExcludeRight --> [']'].
tDot --> ['.'].
tBOL --> ['^'].
tEOL --> ['$'].
