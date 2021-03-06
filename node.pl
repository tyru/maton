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
seq([Q]) --> quant(Q).
seq([Q|Qs]) --> quant(Q), tComma, seq(Qs), {not(empty(Qs))}.

quant(star(A)) --> tStarLeft, group(A), tStarRight.
quant(plus(A)) --> tPlusLeft, group(A), tPlusRight.
quant(option(A)) --> tOptionLeft, group(A), tOptionRight.
quant(repeat(A, N)) --> tRepeatLeft, group(A), tRepeatMiddle, repeat(N), tRepeatRight.
quant(repeat(A, N, M)) --> tRepeatLeft, group(A), tRepeatMiddle, repeat(N, M), tRepeatRight.
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

% {n}
repeat(N) --> digits(N).
% {,}
repeat(nil, nil) --> tNil, tRepeatMiddle, tNil.
% {n,}
repeat(N, nil) --> digits(N), tRepeatMiddle, tNil.
% {,m}
repeat(nil, M) --> tNil, tRepeatMiddle, digits(M).
% {n,m}
repeat(N, M) --> digits(N), tRepeatMiddle, digits(M), {N =< M}.

% 1 or more digits
digits(N) --> parsing, parse_digits(0, N).
digits(N) --> generating, generate_digits(N).

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
charset1([range(A, B) | Xs]) --> range(A, B), comma(Xs), charset1(Xs).
charset1([class(Class) | Xs]) --> class(Class), comma(Xs), charset1(Xs).
charset1([char(C) | Xs]) --> tCharLeft, [C], tCharRight, comma(Xs), charset1(Xs).
charset1([A | Xs]) --> {esc(A)}, [A], comma(Xs), charset1(Xs).

range(A, B) --> tRangeLeft, [A], tComma, [B], tRangeRight.

class(A) --> tClassLeft, lowers(A), tClassRight.

% 1 or more a-z
lowers(A) --> parsing, parse_lowers(A).
lowers(A) --> generating, generate_lowers(A).

generate_lowers(A) --> chars(A).

parse_lowers(A) --> lowers_chars(Cs), {atom_chars(A, Cs)}.
lowers_chars([A]) --> lower(A).
lowers_chars([A | Xs]) --> lower(A), lowers_chars(Xs).

% a-z
lower(A) --> [A], {between(97, 122, N), atom_codes(A, [N])}.

% Add comma if given list is non-empty
comma([]) --> [].
comma([_|_]) --> tComma.

char(dot) --> tDot.
char(bol) --> tBOL.
char(eol) --> tEOL.
char(A) --> {esc(A)}, chars(A).
char(char(C)) --> tCharLeft, [C], tCharRight.

esc(nl).
esc(esc).
esc(tab).
esc(cr).
esc(bs).
esc(digit).
esc(nondigit).

tOrLeft --> chars('or(').
tOrRight --> [')'].
tSeqLeft --> ['['].
tSeqRight --> [']'].
tStarLeft --> chars('star(').
tStarRight --> [')'].
tPlusLeft --> chars('plus(').
tPlusRight --> [')'].
tOptionLeft --> chars('option(').
tOptionRight --> [')'].
tRepeatLeft --> chars('repeat(').
tRepeatMiddle --> [','].
tRepeatRight --> [')'].
tNil --> chars('nil').
tZeroMatchLeft --> chars('zero_match(').
tZeroMatchRight --> [')'].
tZeroNonMatchLeft --> chars('zero_non_match(').
tZeroNonMatchRight --> [')'].
tZeroPredMatchLeft --> chars('zero_pred_match(').
tZeroPredMatchRight --> [')'].
tZeroPredNonMatchLeft --> chars('zero_pred_non_match(').
tZeroPredNonMatchRight --> [')'].
tZeroNoRetryMatchLeft --> chars('zero_no_retry_match(').
tZeroNoRetryMatchRight --> [')'].
tCaptureLeft --> chars('capture(').
tCaptureRight --> [')'].
tGroupLeft --> chars('group(').
tGroupRight --> [')'].
tIncludeLeft --> chars('include([').
tIncludeRight --> chars('])').
tExcludeLeft --> chars('exclude([').
tExcludeRight --> chars('])').
tRangeLeft --> chars('range(').
tRangeRight --> [')'].
tClassLeft --> chars('class(').
tClassRight --> [')'].
tCharLeft --> chars('char(').
tCharRight --> [')'].
tComma --> [','].
tDot --> chars('dot').
tBOL --> chars('bol').
tEOL --> chars('eol').
tEscNL --> chars('nl').
