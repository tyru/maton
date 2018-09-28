:- module(vim, []).
:- consult(regexp).

maton_rule.

:- regexp:export(Clauses), forall(member(C, Clauses), (expand_term(C, C1), assertz(C1))).

tOr --> ['\\', '|'].
tDot --> ['.'].
tStar --> ['*'].
tPlus --> ['\\', '+'].
tOption --> ['\\', '?'].
tGroupLeft --> ['\\', '('].
tGroupRight --> ['\\', ')'].
