/*
 * This rules defines conversion between internal node and
 * Vim regular expression syntax.
 */
:- module(vim, []).
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
quant(zero_match([A])) -->    % a\@=
  tZeroMatchLeft, group(A), tZeroMatchRight.
quant(zero_match(A)) -->    % \%(abc\)\@=
  tZeroMatchLeft, group(group(A)), tZeroMatchRight.
quant(zero_non_match([A])) -->    % a\@!
  tZeroNonMatchLeft, group(A), tZeroNonMatchRight.
quant(zero_non_match(A)) -->    % \%(abc\)\@!
  tZeroNonMatchLeft, group(group(A)), tZeroNonMatchRight.
quant(zero_pred_match([A])) -->    % a\@<=
  tZeroPredMatchLeft, group(A), tZeroPredMatchRight.
quant(zero_pred_match(A)) -->    % \%(abc\)\@<=
  tZeroPredMatchLeft, group(group(A)), tZeroPredMatchRight.
quant(zero_pred_non_match([A])) -->    % a\@<!
  tZeroPredNonMatchLeft, group(A), tZeroPredNonMatchRight.
quant(zero_pred_non_match(A)) -->    % \%(abc\)\@<!
  tZeroPredNonMatchLeft, group(group(A)), tZeroPredNonMatchRight.
quant(zero_no_retry_match([A])) -->    % a\@>
  tZeroNoRetryMatchLeft, group(A), tZeroNoRetryMatchRight.
quant(zero_no_retry_match(A)) -->    % \%(abc\)\@>
  tZeroNoRetryMatchLeft, group(group(A)), tZeroNoRetryMatchRight.
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

charset(exclude(A)) --> tExcludeLeft, charset1(A), tExcludeRight.
charset(include(A)) --> tIncludeLeft, charset1(A), tIncludeRight.
charset(A) --> char(A).

charset1([]) --> [].
charset1([range(A, B) | Xs]) --> [A, '-', B], charset1(Xs).
charset1([char(A), char('-')]) --> [A, '-'].
charset1([A | Xs]) --> ['\\', C], {esc(C, A)}, charset1(Xs).
charset1([char(A) | Xs]) --> ['\\', C], {not(esc(C, _)), atom_concat('\\', C, A)}, charset1(Xs).
charset1([char(A) | Xs]) --> [A], {not(charset_meta(A))}, charset1(Xs).
charset1([class(Class) | Xs]) --> class(Class), charset1(Xs).

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

tOr --> chars('\\|').
tStar --> ['*'].
tPlus --> chars('\\+').
tOption --> chars('\\?').
tOption --> chars('\\=').
tRepeatLeft --> chars('\\{').
tRepeatMiddle --> [','].
tRepeatRight --> ['}'].
tZeroMatchLeft --> [].
tZeroMatchRight --> chars('\\@=').
tZeroNonMatchLeft --> [].
tZeroNonMatchRight --> chars('\\@!').
tZeroPredMatchLeft --> [].
tZeroPredMatchRight --> chars('\\@<=').
tZeroPredNonMatchLeft --> [].
tZeroPredNonMatchRight --> chars('\\@<!').
tZeroNoRetryMatchLeft --> [].
tZeroNoRetryMatchRight --> chars('\\@>').
tCaptureLeft --> chars('\\(').
tCaptureRight --> chars('\\)').
tGroupLeft --> chars('\\%(').
tGroupRight --> chars('\\)').
tIncludeLeft --> ['['].
tIncludeRight --> [']'].
tExcludeLeft --> chars('[^').
tExcludeRight --> [']'].
tDot --> ['.'].
tBOL --> ['^'].
tEOL --> ['$'].
