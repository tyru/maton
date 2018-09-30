:- consult(maton).
:- consult(ereg).
:- consult(vim).
:- consult(node).

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

:- begin_tests(char).
  test(dot, [nondet]) :-
    bidirectionally_convertible([
      ereg = '.',
      vim = '.',
      node = '[dot]'
    ]).
  test(bol, [nondet]) :-
    bidirectionally_convertible([
      ereg = '^',
      vim = '^',
      node = '[bol]'
    ]).
  test(eol, [nondet]) :-
    bidirectionally_convertible([
      ereg = '$',
      vim = '$',
      node = '[eol]'
    ]).
  test(nl, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\n',
      vim = '\\n',
      node = '[nl]'
    ]).
  test(esc, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\e',
      vim = '\\e',
      node = '[esc]'
    ]).
  test(tab, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\t',
      vim = '\\t',
      node = '[tab]'
    ]).
  test(cr, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\r',
      vim = '\\r',
      node = '[cr]'
    ]).
  test(bs, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\b',
      vim = '\\b',
      node = '[bs]'
    ]).
  test(char, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a',
      vim = 'a',
      node = '[char(a)]'
    ]).
  test(esc_a, [nondet]) :-
    bidirectionally_convertible([
      ereg = '\\a',
      vim = '\\a',
      node = '[char(\\a)]'
    ]).
  test('esc_(', [nondet, blocked(wtf)]) :-
    bidirectionally_convertible([
      ereg = '\\(',
      vim = '('
    ]).
:- end_tests(char).

:- begin_tests(group_and_or).
  test(or1, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'a|b',
      vim = 'a\\|b',
      node = 'or([char(a)],[char(b)])'
    ]).
  test(or2, [nondet]) :-
    bidirectionally_convertible([
      ereg = 'ab|c',
      vim = 'ab\\|c',
      node = 'or([char(a),char(b)],[char(c)])'
    ]).
  test(empty_or1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '|b',
      vim = '\\|b',
      node = 'or([],[char(b)])'
    ]).
  test(empty_or2) :-
    bidirectionally_convertible([
      ereg = 'a|',
      vim = 'a\\|',
      node = 'or([char(a)],[])'
    ]).
  test(group1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(a)',
      vim = '\\(a\\)',
      node = '[capture([char(a)])]'
    ]).
  test(group2, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(ab)',
      vim = '\\(ab\\)',
      node = '[capture([char(a),char(b)])]'
    ]).
  test(group3, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(a)(b)',
      vim = '\\(a\\)\\(b\\)',
      node = '[capture([char(a)]),capture([char(b)])]'
    ]).
  test(group4, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(a)b(c)',
      vim = '\\(a\\)b\\(c\\)',
      node = '[capture([char(a)]),char(b),capture([char(c)])]'
    ]).
  test(group_or1, [nondet, true(Node == [char(z),capture(or([char(a)],[char(b)]))]),
                    blocked(wtf)]) :-
    % bidirectionally_convertible([
    %   ereg = 'z(a|b)',
    %   vim = 'z\\(a\\|b\\)',
    %   node = '[char(z),capture(or([char(a)],[char(b)]))]'
    % ]).
    string_chars("z\\(a\\|b\\)", Cs),
    phrase(vim:toplevel(Node), Cs).
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
    bidirectionally_convertible([
      ereg = 'a{,}',
      vim = 'a\\{,}',
      node = '[repeat(char(a),nil,nil)]'
    ]).

  test(zero_match1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?=a)',
      vim = 'a\\@=',
      node = '[zero_match([char(a)])]'
    ]).
  test(zero_match2, [nondet]) :-
    maton:convert(ereg, '(?=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@='),
    maton:convert(vim, '\\%(abc\\)\\@=', ereg, ResEreg),
    assertion(ResEreg = '(?=(?:abc))').

  test(zero_non_match1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?!a)',
      vim = 'a\\@!',
      node = '[zero_non_match([char(a)])]'
    ]).
  test(zero_non_match2, [nondet]) :-
    maton:convert(ereg, '(?!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@!'),
    maton:convert(vim, '\\%(abc\\)\\@!', ereg, ResEreg),
    assertion(ResEreg = '(?!(?:abc))').

  test(zero_pred_match1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?<=a)',
      vim = 'a\\@<=',
      node = '[zero_pred_match([char(a)])]'
    ]).
  test(zero_pred_match2, [nondet]) :-
    maton:convert(ereg, '(?<=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<='),
    maton:convert(vim, '\\%(abc\\)\\@<=', ereg, ResEreg),
    assertion(ResEreg = '(?<=(?:abc))').

  test(zero_pred_non_match1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?<!a)',
      vim = 'a\\@<!',
      node = '[zero_pred_non_match([char(a)])]'
    ]).
  test(zero_pred_non_match2, [nondet]) :-
    maton:convert(ereg, '(?<!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<!'),
    maton:convert(vim, '\\%(abc\\)\\@<!', ereg, ResEreg),
    assertion(ResEreg = '(?<!(?:abc))').

  test(zero_no_retry_match1, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?>a)',
      vim = 'a\\@>',
      node = '[zero_no_retry_match([char(a)])]'
    ]).
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

  test(group, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(?:a)',
      vim = '\\%(a\\)',
      node = '[group([char(a)])]'
    ]).
  test(capture, [nondet]) :-
    bidirectionally_convertible([
      ereg = '(a)',
      vim = '\\(a\\)',
      node = '[capture([char(a)])]'
    ]).
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
