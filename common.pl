:- module(common, [parsing/2, generating/2, eos/2, chars/3, empty/1]).

parsing(A, A) :- nonvar(A), A \= [].
generating(A, B) :- var(A), A = B.
eos([], []).

chars(A) --> {atom_chars(A, Cs)}, char_list(Cs).
char_list([]) --> [].
char_list([C | Cs]) --> [C], char_list(Cs).

empty([]).

not(Goal) --> Goal, !, {fail}.
not(_) --> [].
