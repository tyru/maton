:- module(ereg, [maton_rule/0, tOr/2, tDot/2, tStar/2, tPlus/2, tOption/2, tGroupLeft/2, tGroupRight/2]).
:- consult(regexp).
:- use_module(regexp).

maton_rule.

tOr --> ['|'].
tDot --> ['.'].
tStar --> ['*'].
tPlus --> ['+'].
tOption --> ['?'].
tGroupLeft --> ['('].
tGroupRight --> [')'].
