:- begin_tests(markdown_basics).
:- use_module('markdown/markdown_basics').

test(offset, [nondet]) :-
    string_codes("   Lorem", Codes),
    string_codes("Lorem", ExpectedCodes),
    phrase(markdown_basics:offset(0,5,ActualOffset), Codes, ActualCodes ),
    ActualOffset = 3,
    ActualCodes = ExpectedCodes.

test(offset, [nondet]) :-
    string_codes("   Lorem", Codes),
    string_codes("Lorem", ExpectedCodes),
    phrase(markdown_basics:offset(0,3,ActualOffset), Codes, ActualCodes ),
    ActualOffset = 3,
    ActualCodes = ExpectedCodes.

test(offset, [nondet]) :-
    string_codes("Lorem", Codes),
    string_codes("Lorem", ExpectedCodes),
    phrase(markdown_basics:offset(0,5,ActualOffset), Codes, ActualCodes ),
    ActualOffset = 0,
    ActualCodes = ExpectedCodes.

test(offset, [nondet]) :-
    string_codes("   Lorem", Codes),
    string_codes("Lorem", ExpectedCodes),
    phrase(markdown_basics:offset(_,_,ActualOffset), Codes, ActualCodes ),
    ActualOffset = 3,
    ActualCodes = ExpectedCodes.

test(offset, [fail]) :-
    string_codes("   Lorem", Codes),
    phrase(markdown_basics:offset(0,2,_), Codes, _ ).

test(offset, [fail]) :-
    string_codes("   Lorem", Codes),
    phrase(markdown_basics:offset(_,2,_), Codes, _ ).


test(offset, [fail]) :-
    string_codes("   Lorem", Codes),
    phrase(markdown_basics:offset(4, 10,_), Codes, _ ).

test(offset, [fail]) :-
    string_codes("   Lorem", Codes),
    phrase(markdown_basics:offset(4,_,_), Codes, _ ).
:- end_tests(markdown_basics).