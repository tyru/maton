:- consult(ereg).

:- begin_tests(char).
  test(dot, [nondet, true(Node == [dot])]) :-
    string_chars(".", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(bol, [nondet, true(Node == [bol])]) :-
    string_chars("^", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(eol, [nondet, true(Node == [eol])]) :-
    string_chars("$", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(nl, [nondet, true(Node == [nl])]) :-
    string_chars("\\n", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(esc, [nondet, true(Node == [esc])]) :-
    string_chars("\\e", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(tab, [nondet, true(Node == [tab])]) :-
    string_chars("\\t", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(cr, [nondet, true(Node == [cr])]) :-
    string_chars("\\r", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(bs, [nondet, true(Node == [bs])]) :-
    string_chars("\\b", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(esc, [nondet, true(Node == [char('\\a')])]) :-
    string_chars("\\a", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(char, [nondet, true(Node == [char(a)])]) :-
    string_chars("a", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(char).

:- begin_tests(group_and_or).
  test(or1, [nondet, true(Node == or([char(a)], [char(b)]))]) :-
    string_chars("a|b", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(or2, [nondet, true(Node == or([char(a), char(b)], [char(c)]))]) :-
    string_chars("ab|c", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(empty_or1, [nondet, true(Node == or([], [char(b)]))]) :-
    string_chars("|b", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(empty_or2, [nondet, true(Node == or([char(a)], []))]) :-
    string_chars("a|", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group1, [nondet, true(Node == [group([char(a)])])]) :-
    string_chars("(a)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group2, [nondet, true(Node == [group([char(a), char(b)])])]) :-
    string_chars("(ab)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group3, [nondet, true(Node == [group([char(a)]), group([char(b)])])]) :-
    string_chars("(a)(b)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group4, [nondet, true(Node == [group([char(a)]), char(b), group([char(c)])])]) :-
    string_chars("(a)b(c)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group_or1, [nondet, true(Node == [char(z), group(or([char(a)], [char(b)]))])]) :-
    string_chars("z(a|b)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group_or2, [nondet, true(Node == [char(z), group(or([char(a), char(b)], [char(c)]))])]) :-
    string_chars("z(ab|c)", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(group_and_or).

:- begin_tests(quantifier).
  test(star1, [nondet, true(Node = [star(char(a))])]) :-
    string_chars("a*", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(star2, [nondet, true(Node = [star(group([char(a), char(b)]))])]) :-
    string_chars("(ab)*", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(plus1, [nondet, true(Node = [plus(char(a))])]) :-
    string_chars("a+", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(plus2, [nondet, true(Node = [plus(group([char(a), char(b)]))])]) :-
    string_chars("(ab)+", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(option1, [nondet, true(Node = [option(char(a))])]) :-
    string_chars("a?", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(option2, [nondet, true(Node = [option(group([char(a), char(b)]))])]) :-
    string_chars("(ab)?", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_1, [nondet, true(Node = [repeat(char(a), 1)])]) :-
    string_chars("a{1}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_group_1, [nondet, true(Node = [repeat(group([char(a), char(b)]), 1)])]) :-
    string_chars("(ab){1}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_0, [nondet, true(Node = [repeat(char(a), 0)])]) :-
    string_chars("a{0}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_11, [nondet, true(Node = [repeat(char(a), 11)])]) :-
    string_chars("a{11}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_12, [nondet, true(Node = [repeat(char(a), 1, 2)])]) :-
    string_chars("a{1,2}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_22, [nondet, true(Node = [repeat(char(a), 2, 2)])]) :-
    string_chars("a{2,2}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_11_22, [nondet, true(Node = [repeat(char(a), 11, 22)])]) :-
    string_chars("a{11,22}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_1m, [nondet, true(Node = [repeat(char(a), 1, -1)])]) :-
    string_chars("a{1,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_11m, [nondet, true(Node = [repeat(char(a), 11, -1)])]) :-
    string_chars("a{11,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_n1, [nondet, true(Node = [repeat(char(a), 0, 1)])]) :-
    string_chars("a{,1}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_n11, [nondet, true(Node = [repeat(char(a), 0, 11)])]) :-
    string_chars("a{,11}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_nm, [nondet, true(Node = [repeat(char(a), 0, -1)])]) :-
    string_chars("a{,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(quantifier).

:- begin_tests(include_and_exclude).

  invert_charset(Cs, ExCs) :-
    append([['[' | Middle], [']']], Cs),
    append([['[', '^' | Middle], [']']], ExCs).

  test_include_and_exclude(S, Set) :-
    string_chars(S, Cs),
    phrase(ereg:toplevel(Include), Cs),
    assertion(Include = [include(Set)]),
    invert_charset(Cs, ExCs),
    phrase(ereg:toplevel(Exclude), ExCs),
    assertion(Exclude = [exclude(Set)]).

  test(empty, [nondet]) :-
    test_include_and_exclude("[]", []).
  test(a, [nondet]) :-
    test_include_and_exclude("[a]", [a]).
  test(a_to_z, [nondet]) :-
    test_include_and_exclude("[a-z]", [range(a, z)]).
  test(a_to_z_and_A_to_Z, [nondet]) :-
    test_include_and_exclude("[a-zA-Z]", [range(a, z), range('A', 'Z')]).
  test(a_hyphen, [nondet]) :-
    test_include_and_exclude("[a-]", [a, '-']).
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:]]", [class(lower)]).
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:][:upper:]]", [class(lower), class(upper)]).

  test(dot, [nondet]) :-
    test_include_and_exclude("[.]", ['.']).
  test(star, [nondet]) :-
    test_include_and_exclude("[*]", ['*']).
  test(plus, [nondet]) :-
    test_include_and_exclude("[+]", ['+']).
  test(option, [nondet]) :-
    test_include_and_exclude("[?]", ['?']).
  test(dollar, [nondet]) :-
    test_include_and_exclude("[$]", ['$']).

  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\\\]", ['\\\\']).
  test(hat, [nondet]) :-
    test_include_and_exclude("[\\^]", ['\\^']).
  test(left_bracket, [nondet]) :-
    test_include_and_exclude("[\\[]", ['\\[']).
  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\]]", ['\\]']).

  test(exclude_hat, [nondet, true(Node = [exclude([])])]) :-
    string_chars("[^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(exclude_hat, [nondet, true(Node = [exclude(['^'])])]) :-
    string_chars("[^^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(exclude_hat, [nondet, true(Node = [include([a, '^'])])]) :-
    string_chars("[a^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(include_and_exclude).

:- begin_tests(error).
  test(error1, [fail]) :-
    string_chars("(", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error2, [fail]) :-
    string_chars(")", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error3, [fail]) :-
    string_chars("[", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error4, [fail]) :-
    string_chars("]", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error5, [fail]) :-
    string_chars("{", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error6, [fail]) :-
    string_chars("}", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error7, [fail]) :-
    string_chars("*", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error8, [fail]) :-
    string_chars("+", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error9, [fail]) :-
    string_chars("?", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error10, [fail]) :-
    string_chars("\\", Cs),
    phrase(ereg:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
:- end_tests(error).

:- run_tests.
:- halt.
