:- initialization main.

main :-
  (
    parse_args(Opts, Args),
    loop(Opts, Args)
  ) -> halt(0); halt(1).

opts_spec([
  [opt(from), type(atom),
    shortflags([f]), longflags([from]),
    help(['input format of string'])],
  [opt(to), type(atom),
    shortflags([t]), longflags([to]),
    help(['output format of string'])]
]).

parse_args(Opts, Args) :-
  current_prolog_flag(argv, RawArgs),
  opts_spec(OptsSpec),
  opt_parse(OptsSpec, RawArgs, Opts, Args),
  check_opts(Opts, Err),
  (Err == nil -> true; format('error: ~s\n', [Err]), fail).

check_opts(Opts, Err) :-
  member(from(From), Opts),
  member(to(To), Opts),
  (
    (var(From); var(To)) -> Err = '--from and --to options are required.' ;
    catch(
      (
        consult_rule(From), consult_rule(To), Err = nil
      ), E, (
        get_message(E, S),
        format(atom(Err), 'could not load modules: ~s', [S])
      )
    )
  ).

get_message(E, E) :- atom(E), !.
get_message(E, S) :- message_to_string(E, S).

consult_rule(Module) :-
  % 1. Module name consists of a-z characters.
  (
    string_codes(Module, Cs),
    forall(member(C, Cs), between(97, 122, C)), !;
      throw('module name must consist of a-z characters')
  ),
  % 2. Can load it
  consult(Module),
  % 3. It has maton_rule/0 predicate
  Module:maton_rule.

loop(Opts, Args) :-
  repeat,
  get_input(Args, In, Len),
  convert(Opts, In, Out),
  writeln(Out),
  Len =:= -1.

get_input([X|Xs], S, Len) :- !, get_elems([X|Xs], S, Len).
get_input([], S, Len) :- !, get_stdin_lines(S, Len).

get_elems([X], X, -1) :- !.
get_elems([X|_], X, Len) :- string_length(X, Len).
get_elems([_|Xs], X, Len) :- get_elems(Xs, X, Len).

get_stdin_lines(L, Len) :-
  current_input(Input),
  read_string(Input, "\n", "\r\t ", Len, L).

convert(Opts, In, Out) :-
  member(from(From), Opts),
  member(to(To), Opts),
  convert(From, In, To, Out).
convert(From, In, To, Out) :-
  % In -> Node
  string_chars(In, InCs),
  phrase(From:toplevel(Node), InCs),
  % Node -> Out
  phrase(To:toplevel(Node), OutCs),
  string_chars(Out, OutCs).
