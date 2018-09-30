:- module(common, [when_parsing/4, chars/3]).

%% when_parsing(:Then, :Else)//
%
%  Call Goal when DCG operates in parsing mode.
:- meta_predicate when_parsing(2,2,?,?).
when_parsing(Then, Else) -->
  ( parsing -> call(Then); [], call(Else) ).

%% parsing// is semidet.
%
%  True if DCG is operating as a parser.  Specifically,
%  the DCG list is not a variable.
parsing(H,H) :-
  nonvar(H).

chars(A) --> {atom_chars(A, Cs)}, char_list(Cs).
char_list([]) --> [].
char_list([C | Cs]) --> [C], char_list(Cs).
