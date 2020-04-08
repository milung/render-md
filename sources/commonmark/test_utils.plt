:- begin_tests(test_utils).
:- use_module(source(commonmark/test_utils)).

:- encoding(utf8).


test(are_atomic_equal, [
    forall(member(case(Expected, Actual), [
    case(32, '32'),
    case("expectato", expectato),
    case([some, ' ', "list of results"], "some list of results")
]))]) :-
    % given - case
    % when 
    test_utils:are_atomic_equal(Expected, Actual),
    % then
    true.
test(are_atomic_equal, [
    fail,
    forall(member(case(Expected, Actual), [
    case(32, '33'),
    case("expectato", expectet),
    case([some, "list of results"], "some list of results")
]))]) :-
    % given - case
    % when 
    test_utils:are_atomic_equal(Expected, Actual),
    % then
    true.

test(phrase_atomic_string, [forall(member(case(Input, Rest), [
    case('This is the phrase', ''),
    case(['This ', is, ' the phrase', " with a tail"], ' with a tail')
]))]) :-
    % given - case
    % when
    test_utils:phrase_atomic_string("This is the phrase", Input, Actual),
    % then
    are_atomic_equal(Rest, Actual).


:- end_tests(test_utils).