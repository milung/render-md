:- module(test_utils, [
    are_atomic_equal/2,         % +Expected, +Actual
    are_atomic_equal/3,         % +Expected, +Actual, +Message:string
    are_equal/2,         % +Expected, +Actual
    are_equal/3,         % +Expected, +Actual, +Message:string
    phrase_atomic_string/3      % +Goal:term, ?Input:atomic, ?Tail:atomic    
]).
%! <module> Test utility predicates
%  Predicates for simplifying the unit testing creation

:- encoding(utf8).

:- meta_predicate phrase_atomic_string( //, ?, ?).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! are_atomic_equal(+Expected, +Actual ) is semidet
%  Same as are_atomic_equal/3 with the predefined message
are_atomic_equal(Expected, Actual) :-
    are_atomic_equal(Expected, Actual, "The values do not equal: expected `~w`, but was `~w`").

%! are_atomic_equal(+Expected, +Actual, +Message:string) is semidet
%  Succeeds if the Expected is equivalent to Actual after converting them 
%  to atom. Expected and Actual can be atomic or list of atomic elements.
% If the Expected and Actual do not equal then error Message is written to
% the output. The Message is subject to debug/3 with arguments [Expected, Actual]
are_atomic_equal(Expected, Actual, Message) :-
    flatten([Expected], FlatExpected),
    atomic_list_concat(FlatExpected, ExpectedAtom),
    flatten([Actual], FlatActual),
    atomic_list_concat(FlatActual, ActualAtom),
    are_equal(ExpectedAtom, ActualAtom, Message).

%! are_equal(+Expected, +Actual) is semidet    
%  same as are_equal with predefined message
are_equal(Expected, Actual) :- 
    are_equal(Expected, Actual, "The values do not equal: expected `~w`, but was `~w`").

%! are_equal(+Expected, +Actual, +Message:string) is semidet
% Succeeds if the Expected is equivalent to Actual.
% If the Expected and Actual do not equal then error Message is written to
% the output. The Message is subject to debug/3 with arguments [Expected, Actual]
are_equal(Expected, Expected, _) :- !.
are_equal(Expected, Actual, ErrorMessage) :-
    debug(error(unit_test), ErrorMessage, [Expected, Actual]),
    fail.

%! phrase_atomic_string(+Goal:term, ?Input:atomic, ?Tail:atomic) is nondet
%  Same as phrase/3 but accepts atomics or atomic lists instead of codes. 
phrase_atomic_string(Goal, Input, Tail) :-
    flatten([Input], FlatInputs),
    atomic_list_concat(FlatInputs, InputAtom),
    string_codes(InputAtom, Codes),
    phrase(Goal, Codes, RestCodes),
    string_codes(Tail, RestCodes).
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%