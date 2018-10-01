:- consult(maton).
:- consult(ere).
:- consult(vim).
:- consult(node).

pair(M1 = P1, M2 = P2, L) :-
  member(M1 = P1, L), member(M2 = P2, L), compare(<, M1, M2).
bidirectionally_convertible(L) :-
  length(L, N), N >= 2,
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
      ere = '.',
      vim = '.',
      node = '[dot]'
    ]).
  test(bol, [nondet]) :-
    bidirectionally_convertible([
      ere = '^',
      vim = '^',
      node = '[bol]'
    ]).
  test(eol, [nondet]) :-
    bidirectionally_convertible([
      ere = '$',
      vim = '$',
      node = '[eol]'
    ]).
  test(nl, [nondet]) :-
    bidirectionally_convertible([
      ere = '\\n',
      vim = '\\n',
      node = '[nl]'
    ]).
  test(esc, [nondet]) :-
    bidirectionally_convertible([
      ere = '\\e',
      vim = '\\e',
      node = '[esc]'
    ]).
  test(tab, [nondet]) :-
    bidirectionally_convertible([
      ere = '\\t',
      vim = '\\t',
      node = '[tab]'
    ]).
  test(cr, [nondet]) :-
    bidirectionally_convertible([
      ere = '\\r',
      vim = '\\r',
      node = '[cr]'
    ]).
  test(bs, [nondet]) :-
    bidirectionally_convertible([
      ere = '\\b',
      vim = '\\b',
      node = '[bs]'
    ]).
  test(char, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a',
      vim = 'a',
      node = '[char(a)]'
    ]).
  test('esc_(', [nondet]) :-
    bidirectionally_convertible([
      ere = '\\(',
      vim = '(',
      node = '[char(()]'
    ]).
:- end_tests(char).

:- begin_tests(group_and_or).
  test(or1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a|b',
      vim = 'a\\|b',
      node = 'or([char(a)],[char(b)])'
    ]).
  test(or2, [nondet]) :-
    bidirectionally_convertible([
      ere = 'ab|c',
      vim = 'ab\\|c',
      node = 'or([char(a),char(b)],[char(c)])'
    ]).
  test(empty_or1, [nondet]) :-
    bidirectionally_convertible([
      ere = '|b',
      vim = '\\|b',
      node = 'or([],[char(b)])'
    ]).
  test(empty_or2) :-
    bidirectionally_convertible([
      ere = 'a|',
      vim = 'a\\|',
      node = 'or([char(a)],[])'
    ]).
  test(group1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(a)',
      vim = '\\(a\\)',
      node = '[capture([char(a)])]'
    ]).
  test(group2, [nondet]) :-
    bidirectionally_convertible([
      ere = '(ab)',
      vim = '\\(ab\\)',
      node = '[capture([char(a),char(b)])]'
    ]).
  test(group3, [nondet]) :-
    bidirectionally_convertible([
      ere = '(a)(b)',
      vim = '\\(a\\)\\(b\\)',
      node = '[capture([char(a)]),capture([char(b)])]'
    ]).
  test(group4, [nondet]) :-
    bidirectionally_convertible([
      ere = '(a)b(c)',
      vim = '\\(a\\)b\\(c\\)',
      node = '[capture([char(a)]),char(b),capture([char(c)])]'
    ]).
  test(group_or1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'z(a|b)',
      vim = 'z\\(a\\|b\\)',
      node = '[char(z),capture(or([char(a)],[char(b)]))]'
    ]).
  test(group_or2, [nondet]) :-
    bidirectionally_convertible([
      ere = 'z(ab|c)',
      vim = 'z\\(ab\\|c\\)',
      node = '[char(z),capture(or([char(a),char(b)],[char(c)]))]'
    ]).
:- end_tests(group_and_or).

:- begin_tests(quantifier).
  test(star1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a*',
      vim = 'a*',
      node = '[star(char(a))]'
    ]).
  test(star2, [nondet]) :-
    bidirectionally_convertible([
      ere = '(ab)*',
      vim = '\\(ab\\)*',
      node = '[star(capture([char(a),char(b)]))]'
    ]).
  test(plus1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a+',
      vim = 'a\\+',
      node = '[plus(char(a))]'
    ]).
  test(plus2, [nondet]) :-
    bidirectionally_convertible([
      ere = '(ab)+',
      vim = '\\(ab\\)\\+',
      node = '[plus(capture([char(a),char(b)]))]'
    ]).
  test(option1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a?',
      vim = 'a\\?',
      node = '[option(char(a))]'
    ]).
  test(option2, [nondet]) :-
    bidirectionally_convertible([
      ere = '(ab)?',
      vim = '\\(ab\\)\\?',
      node = '[option(capture([char(a),char(b)]))]'
    ]).
  test(repeat_a_1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{1}',
      vim = 'a\\{1}',
      node = '[repeat(char(a),1)]'
    ]).
  test(repeat_group_1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(ab){1}',
      vim = '\\(ab\\)\\{1}',
      node = '[repeat(capture([char(a),char(b)]),1)]'
    ]).
  test(repeat_a_0, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{0}',
      vim = 'a\\{0}',
      node = '[repeat(char(a),0)]'
    ]).
  test(repeat_a_11, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{11}',
      vim = 'a\\{11}',
      node = '[repeat(char(a),11)]'
    ]).
  test(repeat_a_12, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{1,2}',
      vim = 'a\\{1,2}',
      node = '[repeat(char(a),1,2)]'
    ]).
  test(repeat_a_22, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{2,2}',
      vim = 'a\\{2,2}',
      node = '[repeat(char(a),2,2)]'
    ]).
  test(repeat_a_11_22, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{11,22}',
      vim = 'a\\{11,22}',
      node = '[repeat(char(a),11,22)]'
    ]).
  test(repeat_a_1m, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{1,}',
      vim = 'a\\{1,}',
      node = '[repeat(char(a),1,nil)]'
    ]).
  test(repeat_a_11m, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{11,}',
      vim = 'a\\{11,}',
      node = '[repeat(char(a),11,nil)]'
    ]).
  test(repeat_a_n1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{,1}',
      vim = 'a\\{,1}',
      node = '[repeat(char(a),nil,1)]'
    ]).
  test(repeat_a_n11, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{,11}',
      vim = 'a\\{,11}',
      node = '[repeat(char(a),nil,11)]'
    ]).
  test(repeat_a_nm, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{,}',
      vim = 'a\\{,}',
      node = '[repeat(char(a),nil,nil)]'
    ]).
:- end_tests(quantifier).

:- begin_tests(include_and_exclude).

  regexp_rules([ere, vim]).
  invert_charset(Cs, ExCs) :-
    append(['[' | Middle], [']'], Cs),
    append(['[', '^' | Middle], [']'], ExCs).
  add_pattern(Cs, M, M = A) :- atom_chars(A, Cs).
  test_include_and_exclude(Pat, Set) :-
    regexp_rules(Mods),
    test_include_and_exclude(Mods, Pat, Set).
  test_include_and_exclude(Mods, Pat, Set) :-
    string_chars(Pat, Cs),
    invert_charset(Cs, ExCs),
    format(atom(Include), '[include(~s)]', [Set]),
    format(atom(Exclude), '[exclude(~s)]', [Set]),
    maplist(add_pattern(Cs), Mods, L1),
    append(L1, [node = Include], IncludeList),
    maplist(add_pattern(ExCs), Mods, L2),
    append(L2, [node = Exclude], ExcludeList),
    bidirectionally_convertible(IncludeList),
    bidirectionally_convertible(ExcludeList).

  test(empty, [nondet]) :-
    test_include_and_exclude("[]", '[]').
  test(a, [nondet]) :-
    test_include_and_exclude("[a]", '[char(a)]').
  test(a_to_z, [nondet]) :-
    test_include_and_exclude("[a-z]", '[range(a,z)]').
  test(a_to_z_and_A_to_Z, [nondet]) :-
    test_include_and_exclude("[a-zA-Z]", '[range(a,z),range(A,Z)]').
  test(a_hyphen, [nondet]) :-
    test_include_and_exclude("[a-]", '[char(a),char(-)]').
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:]]", '[class(lower)]').
  test(class, [nondet]) :-
    test_include_and_exclude("[[:lower:][:upper:]]", '[class(lower),class(upper)]').

  test(dot, [nondet]) :-
    test_include_and_exclude("[.]", '[char(.)]').
  test(star, [nondet]) :-
    test_include_and_exclude("[*]", '[char(*)]').
  test(plus, [nondet]) :-
    test_include_and_exclude("[+]", '[char(+)]').
  test(option, [nondet]) :-
    test_include_and_exclude("[?]", '[char(?)]').
  test(dollar, [nondet]) :-
    test_include_and_exclude("[$]", '[char($)]').

  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\\\]", '[char(\\)]').
  test(left_bracket, [nondet]) :-
    test_include_and_exclude("[\\[]", '[char([)]').
  test(right_bracket, [nondet]) :-
    test_include_and_exclude("[\\]]", '[char(])]').

  test(include_hat1, [nondet]) :-
    bidirectionally_convertible([
      ere = '[\\^]',
      vim = '[\\^]',
      node = '[include([char(^)])]'
    ]).
  test(exclude_hat1, [nondet]) :-
    bidirectionally_convertible([
      ere = '[^]',
      vim = '[^]',
      node = '[exclude([])]'
    ]).
  test(exclude_hat2, [nondet]) :-
    bidirectionally_convertible([
      ere = '[^^]',
      vim = '[^^]',
      node = '[exclude([char(^)])]'
    ]).
  test(exclude_hat3, [nondet]) :-
    bidirectionally_convertible([
      ere = '[a^]',
      vim = '[a^]',
      node = '[include([char(a),char(^)])]'
    ]).
:- end_tests(include_and_exclude).

:- begin_tests(conversion).

  test(or, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a|b',
      vim = 'a\\|b',
      node = 'or([char(a)],[char(b)])'
    ]).
  test(star, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a*',
      vim = 'a*',
      node = '[star(char(a))]'
    ]).
  test(plus, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a+',
      vim = 'a\\+',
      node = '[plus(char(a))]'
    ]).

  test(option1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a?',
      vim = 'a\\?',
      node = '[option(char(a))]'
    ]).
  test(option2, [nondet]) :-
    maton:convert(vim, 'a\\?', ere, Res1),
    assertion(Res1 = 'a?'),
    maton:convert(vim, 'a\\=', ere, Res2),
    assertion(Res2 = 'a?').

  test(repeat1, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{1,2}',
      vim = 'a\\{1,2}',
      node = '[repeat(char(a),1,2)]'
    ]).
  test(repeat2, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{,2}',
      vim = 'a\\{,2}',
      node = '[repeat(char(a),nil,2)]'
    ]).
  test(repeat3, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{1,}',
      vim = 'a\\{1,}',
      node = '[repeat(char(a),1,nil)]'
    ]).
  test(repeat4, [nondet]) :-
    bidirectionally_convertible([
      ere = 'a{,}',
      vim = 'a\\{,}',
      node = '[repeat(char(a),nil,nil)]'
    ]).

  test(zero_match1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?=a)',
      vim = 'a\\@=',
      node = '[zero_match([char(a)])]'
    ]).
  test(zero_match2, [nondet]) :-
    maton:convert(ere, '(?=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@='),
    maton:convert(vim, '\\%(abc\\)\\@=', ere, ResEreg),
    assertion(ResEreg = '(?=(?:abc))').

  test(zero_non_match1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?!a)',
      vim = 'a\\@!',
      node = '[zero_non_match([char(a)])]'
    ]).
  test(zero_non_match2, [nondet]) :-
    maton:convert(ere, '(?!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@!'),
    maton:convert(vim, '\\%(abc\\)\\@!', ere, ResEreg),
    assertion(ResEreg = '(?!(?:abc))').

  test(zero_pred_match1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?<=a)',
      vim = 'a\\@<=',
      node = '[zero_pred_match([char(a)])]'
    ]).
  test(zero_pred_match2, [nondet]) :-
    maton:convert(ere, '(?<=abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<='),
    maton:convert(vim, '\\%(abc\\)\\@<=', ere, ResEreg),
    assertion(ResEreg = '(?<=(?:abc))').

  test(zero_pred_non_match1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?<!a)',
      vim = 'a\\@<!',
      node = '[zero_pred_non_match([char(a)])]'
    ]).
  test(zero_pred_non_match2, [nondet]) :-
    maton:convert(ere, '(?<!abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@<!'),
    maton:convert(vim, '\\%(abc\\)\\@<!', ere, ResEreg),
    assertion(ResEreg = '(?<!(?:abc))').

  test(zero_no_retry_match1, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?>a)',
      vim = 'a\\@>',
      node = '[zero_no_retry_match([char(a)])]'
    ]).
  test(zero_no_retry_match2, [nondet]) :-
    maton:convert(ere, '(?>abc)', vim, ResVim),
    assertion(ResVim = '\\%(abc\\)\\@>'),
    maton:convert(vim, '\\%(abc\\)\\@>', ere, ResEreg),
    assertion(ResEreg = '(?>(?:abc))').

  test(include1, [nondet]) :-
    bidirectionally_convertible([
      ere = '[a]',
      vim = '[a]',
      node = '[include([char(a)])]'
    ]).
  test(include2, [nondet]) :-
    bidirectionally_convertible([
      ere = '[[:lower:]]',
      vim  = '[[:lower:]]',
      node = '[include([class(lower)])]'
    ]).
  test(exclude, [nondet]) :-
    bidirectionally_convertible([
      ere = '[^a]',
      vim  = '[^a]',
      node = '[exclude([char(a)])]'
    ]).

  test(group, [nondet]) :-
    bidirectionally_convertible([
      ere = '(?:a)',
      vim = '\\%(a\\)',
      node = '[group([char(a)])]'
    ]).
  test(capture, [nondet]) :-
    bidirectionally_convertible([
      ere = '(a)',
      vim = '\\(a\\)',
      node = '[capture([char(a)])]'
    ]).
:- end_tests(conversion).

:- begin_tests(complex_patterns).
  test(ip_address, [nondet]) :-
    bidirectionally_convertible([
      ere = '^(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]\\d|\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]\\d|\\d)){3}$',
      vim = '^\\%(25[0-5]\\|2[0-4]\\d\\|1\\d\\d\\|[1-9]\\d\\|\\d\\)\\%(\\.\\%(25[0-5]\\|2[0-4]\\d\\|1\\d\\d\\|[1-9]\\d\\|\\d\\)\\)\\{3}$',
      node = '[bol,group(or([char(2),char(5),include([range(0,5)])],or([char(2),include([range(0,4)]),digit],or([char(1),digit,digit],or([include([range(1,9)]),digit],[digit]))))),repeat(group([char(.),group(or([char(2),char(5),include([range(0,5)])],or([char(2),include([range(0,4)]),digit],or([char(1),digit,digit],or([include([range(1,9)]),digit],[digit])))))]),3),eol]'
    ]).
:- end_tests(complex_patterns).

:- begin_tests(error).
  test(error_1_ereg, [fail]) :-
    string_chars("(", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_1_vim, [fail]) :-
    string_chars("\\(", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_2_ereg, [fail]) :-
    string_chars("[", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_2_vim, [fail]) :-
    string_chars("[", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_3_ereg, [fail]) :-
    string_chars("{", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_3_ereg, [fail]) :-
    string_chars("\\{", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_4_ereg, [fail]) :-
    string_chars("*", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_4_vim, [fail]) :-
    string_chars("*", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_5_ereg, [fail]) :-
    string_chars("+", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_5_vim, [fail]) :-
    string_chars("\\+", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_6_ereg, [fail]) :-
    string_chars("?", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_6_vim, [fail]) :-
    string_chars("\\?", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_7_ereg, [fail]) :-
    string_chars("\\", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_7_vim, [fail]) :-
    string_chars("\\", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_8_ereg, [fail]) :-
    string_chars("\\a", Cs),
    phrase(ere:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
  test(error_8_vim, [fail]) :-
    string_chars("\\a", Cs),
    phrase(vim:toplevel(Node), Cs),
    format('Node = ~q~n', [Node]).
:- end_tests(error).

:- run_tests.
:- halt.
