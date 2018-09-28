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
        % TODO Allow only rule files
        consult(From), consult(To), Err = nil
      ), E, (
        message_to_string(E, S),
        format(atom(Err), 'could not load ~s', [S])
      )
    )
  ).

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
  string_chars(In, Cs),
  phrase(From:toplevel(Node), Cs),
  phrase(To:toplevel(Node), Out).
