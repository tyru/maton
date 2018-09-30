:- consult(maton).
:- consult(ereg).
:- consult(vim).
:- consult(node).

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
  test(group1, [nondet, true(Node == [capture([char(a)])])]) :-
    string_chars("(a)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group2, [nondet, true(Node == [capture([char(a), char(b)])])]) :-
    string_chars("(ab)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group3, [nondet, true(Node == [capture([char(a)]), capture([char(b)])])]) :-
    string_chars("(a)(b)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group4, [nondet, true(Node == [capture([char(a)]), char(b), capture([char(c)])])]) :-
    string_chars("(a)b(c)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group_or1, [nondet, true(Node == [char(z), capture(or([char(a)], [char(b)]))])]) :-
    string_chars("z(a|b)", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(group_or2, [nondet, true(Node == [char(z), capture(or([char(a), char(b)], [char(c)]))])]) :-
    string_chars("z(ab|c)", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(group_and_or).

:- begin_tests(quantifier).
  test(star1, [nondet, true(Node = [star(char(a))])]) :-
    string_chars("a*", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(star2, [nondet, true(Node = [star(capture([char(a), char(b)]))])]) :-
    string_chars("(ab)*", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(plus1, [nondet, true(Node = [plus(char(a))])]) :-
    string_chars("a+", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(plus2, [nondet, true(Node = [plus(capture([char(a), char(b)]))])]) :-
    string_chars("(ab)+", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(option1, [nondet, true(Node = [option(char(a))])]) :-
    string_chars("a?", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(option2, [nondet, true(Node = [option(capture([char(a), char(b)]))])]) :-
    string_chars("(ab)?", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_1, [nondet, true(Node = [repeat(char(a), 1)])]) :-
    string_chars("a{1}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_group_1, [nondet, true(Node = [repeat(capture([char(a), char(b)]), 1)])]) :-
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
  test(repeat_a_1m, [nondet, true(Node = [repeat(char(a), 1, nil)])]) :-
    string_chars("a{1,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_11m, [nondet, true(Node = [repeat(char(a), 11, nil)])]) :-
    string_chars("a{11,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_n1, [nondet, true(Node = [repeat(char(a), nil, 1)])]) :-
    string_chars("a{,1}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_n11, [nondet, true(Node = [repeat(char(a), nil, 11)])]) :-
    string_chars("a{,11}", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(repeat_a_nm, [nondet, true(Node = [repeat(char(a), nil, nil)])]) :-
    string_chars("a{,}", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(quantifier).

regexp_rules([ereg, vim]).

:- begin_tests(include_and_exclude).

  invert_charset(Cs, ExCs) :-
    append(['[' | Middle], [']'], Cs),
    append(['[', '^' | Middle], [']'], ExCs).

  test_include_and_exclude(S, Set) :-
    regexp_rules(Mods),
    test_include_and_exclude(Mods, S, Set).
  test_include_and_exclude(Mods, S, Set) :-
    forall(
      member(M, Mods),
      (
        string_chars(S, Cs),
        phrase(M:toplevel(Include), Cs),
        assertion(Include = [include(Set)]),
        invert_charset(Cs, ExCs),
        phrase(M:toplevel(Exclude), ExCs),
        assertion(Exclude = [exclude(Set)])
      )
    ).

  test(empty, [nondet]) :-
    test_include_and_exclude("[]", []).
  test(a, [nondet]) :-
    test_include_and_exclude("[a]", [char(a)]).
  test(a_to_z, [nondet]) :-
    test_include_and_exclude("[a-z]", [range(a, z)]).
  test(a_to_z_and_A_to_Z, [nondet]) :-
    test_include_and_exclude("[a-zA-Z]", [range(a, z), range('A', 'Z')]).
  test(a_hyphen, [nondet]) :-
    test_include_and_exclude("[a-]", [char(a), char('-')]).
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:]]", [class(lower)]).
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:][:upper:]]", [class(lower), class(upper)]).

  test(dot, [nondet]) :-
    test_include_and_exclude("[.]", [char('.')]).
  test(star, [nondet]) :-
    test_include_and_exclude("[*]", [char('*')]).
  test(plus, [nondet]) :-
    test_include_and_exclude("[+]", [char('+')]).
  test(option, [nondet]) :-
    test_include_and_exclude("[?]", [char('?')]).
  test(dollar, [nondet]) :-
    test_include_and_exclude("[$]", [char('$')]).

  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\\\]", [char('\\\\')]).
  test(hat, [nondet]) :-
    test_include_and_exclude("[\\^]", [char('\\^')]).
  test(left_bracket, [nondet]) :-
    test_include_and_exclude("[\\[]", [char('\\[')]).
  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\]]", [char('\\]')]).

  test(exclude_hat, [nondet, true(Node = [exclude([])])]) :-
    string_chars("[^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(exclude_hat, [nondet, true(Node = [exclude([char('^')])])]) :-
    string_chars("[^^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
  test(exclude_hat, [nondet, true(Node = [include([char(a), char('^')])])]) :-
    string_chars("[a^]", Cs),
    phrase(ereg:toplevel(Node), Cs).
:- end_tests(include_and_exclude).

:- begin_tests(conversion).
  pair(M1 = P1, M2 = P2, L) :- member(M1 = P1, L), member(M2 = P2, L), compare(<, M1, M2).
  bidirectionally_convertible(L) :-
    forall(
      pair(M1 = P1, M2 = P2, L),
      bidirectionally_convertible(M1, P1, M2, P2)
    ).
  bidirectionally_convertible(ModA, PatA, ModB, PatB) :-
    maton:convert(ModA, PatA, ModB, ResB),
    assertion(ResB = PatB),
    maton:convert(ModB, PatB, ModA, ResA),
    assertion(ResA = PatA).

  test(or, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a|b',
      vim = 'a\\|b',
      node = 'or([char(a)],[char(b)])'
    ]).
  test(star, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a*',
      vim = 'a*',
      node = '[star(char(a))]'
    ]).
  test(plus, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a+',
      vim = 'a\\+',
      node = '[plus(char(a))]'
    ]).

  test(option1, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a?',
      vim = 'a\\?',
      node = '[option(char(a))]'
    ]).
  test(option2, [nondet]) :-
    maton:convert(vim, 'a\\?', ereg, Res1),
    assertion(Res1 = 'a?'),
    maton:convert(vim, 'a\\=', ereg, Res2),
    assertion(Res2 = 'a?').

  test(repeat1, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a{1,2}',
      vim = 'a\\{1,2}',
      node = '[repeat(char(a),1,2)]'
    ]).
  test(repeat2, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a{,2}',
      vim = 'a\\{,2}',
      node = '[repeat(char(a),nil,2)]'
    ]).
  test(repeat3, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a{1,}',
      vim = 'a\\{1,}',
      node = '[repeat(char(a),1,nil)]'
    ]).
  test(repeat4, [nondet]) :-
    bidirectionally_convertible([ereg = 'a{,}', vim = 'a\\{,}']).

  test(zero_match1, [nondet]) :-
    bidirectionally_convertible([ereg = '(?=a)', vim = 'a\\@=']).
  test(zero_match2, [nondet]) :-
    maton:convert(ereg, '(?=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@='),
    maton:convert(vim, '\\%(abc\\)\\@=', ereg, ResEreg),
    assertion(ResEreg = '(?=(?:abc))').

  test(zero_non_match1, [nondet]) :-
    bidirectionally_convertible([ereg = '(?!a)', vim = 'a\\@!']).
  test(zero_non_match2, [nondet]) :-
    maton:convert(ereg, '(?!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@!'),
    maton:convert(vim, '\\%(abc\\)\\@!', ereg, ResEreg),
    assertion(ResEreg = '(?!(?:abc))').

  test(zero_pred_match1, [nondet]) :-
    bidirectionally_convertible([ereg = '(?<=a)', vim = 'a\\@<=']).
  test(zero_pred_match2, [nondet]) :-
    maton:convert(ereg, '(?<=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<='),
    maton:convert(vim, '\\%(abc\\)\\@<=', ereg, ResEreg),
    assertion(ResEreg = '(?<=(?:abc))').

  test(zero_pred_non_match1, [nondet]) :-
    bidirectionally_convertible([ereg = '(?<!a)', vim = 'a\\@<!']).
  test(zero_pred_non_match2, [nondet]) :-
    maton:convert(ereg, '(?<!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<!'),
    maton:convert(vim, '\\%(abc\\)\\@<!', ereg, ResEreg),
    assertion(ResEreg = '(?<!(?:abc))').

  test(zero_no_retry_match1, [nondet]) :-
    bidirectionally_convertible([ereg = '(?>a)', vim = 'a\\@>']).
  test(zero_no_retry_match2, [nondet]) :-
    maton:convert(ereg, '(?>abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@>'),
    maton:convert(vim, '\\%(abc\\)\\@>', ereg, ResEreg),
    assertion(ResEreg = '(?>(?:abc))').

  test(include1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '[a]',
      vim = '[a]',
      node = '[include([char(a)])]'
    ]).
  test(include2, [nondet]) :-
    bidirectionally_convertible([
      ereg = '[[:lower:]]',
      vim  = '[[:lower:]]',
      node = '[include([class(lower)])]'
    ]).
  test(exclude, [nondet]) :-
    bidirectionally_convertible([
      ereg = '[^a]',
      vim  = '[^a]',
      node = '[exclude([char(a)])]'
    ]).

  test(capture, [nondet]) :-
    bidirectionally_convertible([ereg = '(?:a)', vim = '\\%(a\\)']).
  test(group, [nondet]) :-
    bidirectionally_convertible([ereg = '(a)', vim = '\\(a\\)']).
:- end_tests(conversion).


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
