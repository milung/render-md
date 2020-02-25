:- module(markdown_basics, [
        line_ending//0, 
        space_char//0,
        offset//3,          % ?Min:integer, ?Max:integer, -Actual:integer
        one_or_more//1      % +Code:code
    ]).

:- use_module( library(dcg/basics)).

%! line_ending// is det.
%
% matches end of line variants.
line_ending --> "\r", "\n", !.
line_ending --> "\n", !.

%! space_char// is det.
% 
% matches space character only. 
space_char --> " ". 

%! offset(?Min:integer, ?Max:integer, -Actual:integer )// is nondet.
%
% Succeeds if stream is sequence of the Actual number spaces, where 
% `Min <= Actual <= Max`. Min and Max may be unbound
offset(Min, Max, Actual) -->
    offset(Min, Max, Actual, 0).

offset(Min, Max, Actual, Current) -->
    space_char, !,
    { 
        Next is Current + 1, 
        (
            var(Max)
        ;
            nonvar(Max),
            !, Next =< Max
        )
    },
    offset(Min, Max, Actual, Next).

offset(Min, _, Actual, Actual) -->
    [],
    {
        var(Min)
    ;
        nonvar(Min),
        Actual >= Min
    }.

%! one_or_more( +Code:code ) is det.
%
% Consumes as long sequence of Code characters as possible, succeeds if 
% there is at minimum one such character.
one_or_more(Code) -->
    [ Code ],
    one_or_more(Code),
    !.
one_or_more(Code) -->
    [ Code ].