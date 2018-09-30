/*
 * This rules defines conversion between internal node and
 * textual representation which can be recognizable as Prolog term.
 */
:- module(node, []).
:- consult(common).
:- use_module(common).

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
quant(repeat(A, nil, nil)) -->    % {,}
  tRepeatLeft, group(A), tRepeatMiddle, tNil, tRepeatMiddle, tNil, tRepeatRight.
quant(repeat(A, nil, M)) -->    % {,m}
  tRepeatLeft, group(A), tRepeatMiddle, tNil, tRepeatMiddle, digits(M), tRepeatRight.
quant(repeat(A, N, nil)) -->    % {n,}
  tRepeatLeft, group(A), tRepeatMiddle, digits(N), tRepeatMiddle, tNil, tRepeatRight.
quant(repeat(A, N, M)) -->    % {n,m}
  tRepeatLeft, group(A), tRepeatMiddle, digits(N), tRepeatMiddle, digits(M), tRepeatRight,
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
digits(-N) --> ['-'], when_parsing(parse_digits(0, N), generate_digits(N)).
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
charset(A) --> tCharLeft, char(A), tCharRight.

charset1([]) --> [].
charset1([range(A, B) | Xs]) --> range(A, B), charset1(Xs).
charset1([class(Class) | Xs]) --> class(Class), charset1(Xs).
charset1([A | Xs]) --> tCharLeft, cs_char(A), tCharRight, charset1(Xs).

range(A, B) --> tRangeLeft, [A], tComma, [B], tRangeRight.

cs_char(A) --> ['\\', C], {esc(C, A)}.
cs_char(char(A)) --> ['\\', C], {not(esc(C, _)), atom_concat('\\', C, A)}.
cs_char(char(C)) --> [C], {not(charset_meta(C))}.

class(A) --> tClassLeft, lowers(A), tClassRight.

% 1 or more a-z
lowers(A) --> when_parsing(parse_lowers(A), generate_lowers(A)).

generate_lowers(A) --> {atom_chars(A, Cs)}, chars(Cs).
chars([]) --> [].
chars([C | Cs]) --> [C], chars(Cs).

parse_lowers(A) --> lowers_chars(Cs), {atom_chars(A, Cs)}.
lowers_chars([A]) --> lower(A).
lowers_chars([A | Xs]) --> lower(A), lowers_chars(Xs).

% a-z
lower(A) --> [A], {between(97, 122, N), atom_codes(A, [N])}.

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
tNil --> ['n', 'i', 'l'].
tZeroMatchLeft --> ['z', 'e', 'r', 'o', '_', 'm', 'a', 't', 'c', 'h', '('].
tZeroMatchRight --> [')'].
tZeroNonMatchLeft --> ['z', 'e', 'r', 'o', '_', 'n', 'o', 'n', '_', 'm', 'a', 't', 'c', 'h', '('].
tZeroNonMatchRight --> [')'].
tZeroPredMatchLeft --> ['z', 'e', 'r', 'o', '_', 'p', 'r', 'e', 'd', '_', 'm', 'a', 't', 'c', 'h', '('].
tZeroPredMatchRight --> [')'].
tZeroPredNonMatchLeft --> ['z', 'e', 'r', 'o', '_', 'p', 'r', 'e', 'd', '_', 'n', 'o', 'n', '_', 'm', 'a', 't', 'c', 'h', '('].
tZeroPredNonMatchRight --> [')'].
tZeroNoRetryMatchLeft --> ['z', 'e', 'r', 'o', '_', 'n', 'o', '_', 'r', 'e', 't', 'r', 'y', '_', 'm', 'a', 't', 'c', 'h', '('].
tZeroNoRetryMatchRight --> [')'].
tCaptureLeft --> ['c', 'a', 'p', 't', 'u', 'r', 'e', '('].
tCaptureRight --> [')'].
tGroupLeft --> ['g', 'r', 'o', 'u', 'p', '('].
tGroupRight --> [')'].
tIncludeLeft --> ['i', 'n', 'c', 'l', 'u', 'd', 'e', '(', '['].
tIncludeRight --> [']', ')'].
tExcludeLeft --> ['e', 'x', 'c', 'l', 'u', 'd', 'e', '(', '['].
tExcludeRight --> [']', ')'].
tRangeLeft --> ['r', 'a', 'n', 'g', 'e', '('].
tRangeRight --> [')'].
tClassLeft --> ['c', 'l', 'a', 's', 's', '('].
tClassRight --> [')'].
tCharLeft --> ['c', 'h', 'a', 'r', '('].
tCharRight --> [')'].
tComma --> [','].
tDot --> ['d', 'o', 't'].
tBOL --> ['b', 'o', 'l'].
tEOL --> ['e', 'o', 'l'].
tEscNL --> ['n', 'l'].
