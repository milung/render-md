:- begin_tests(wildcards).
:- use_module(source(commonmark/wildcards)).
:- use_module(source(commonmark/test_utils)).

:- encoding(utf8).

test(at_least_count, [
    nondet,
    forall(member(case(Input, ExpectedCount, ExpectedRest ), [        
        case("test test ", 2, ""),
        case("test test test ", 3, ""),
        case("test test alpha", 2, "alpha"),
        case("test test test alpha test ", 3, "alpha test ")
]))]) :-
    % given - case
    % when
    phrase_atomic_string(wildcards:at_least_count(2, "test ", ActualCount), Input, ActualRest),
    % then
    are_atomic_equal(ExpectedRest, ActualRest),
    are_equal(ExpectedCount, ActualCount).


test(at_least_count, [
    fail,
    forall(member(Input, ["test ","test alpha test ","test" ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(wildcards:at_least_count(2, "test ", _), Input, _).
    % then -fail
    

test(count_or_less, [
    nondet,
    forall(member(case(Input, ExpectedRest ), [        
        case("test test ", ""),
        case("test test test ", "test "),
        case("test test alpha", "alpha"),
        case("test alpha", "alpha"),
        case("alpha", "alpha"),
        case("\n", "\n"),
        case("")
]))]) :-
    % given - case
    % when
    phrase_atomic_string(wildcards:count_or_less(2, "test "), Input, ActualRest),
    % then
    are_atomic_equal(ExpectedRest, ActualRest).

test(one_or_more, [
    nondet,
    forall(member(case(Input, ExpectedRest ), [
        case("test ", ""),
        case("test test ", ""),
        case("test test test ", ""),
        case("test test alpha", "alpha"),
        case("test test alpha test ", "alpha test ")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_more("test "), Codes, ActualRest),
    % then
    string_codes(ExpectedRest, ActualRest).

test(one_or_more, [
    fail,
    forall(member(Input, [
        "test", "", "alpha test ", "   test "
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_more("test "), Codes, _)
    % then fails
    . 

test(one_or_max, [
    nondet,
    forall(member(case(Input, ExpectedRest ), [
        case("test ", ""),
        case("test test ", ""),
        case("test test alpha", "alpha"),
        case("test test alpha test ", "alpha test ")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_max(2, "test "), Codes, ActualRest),
    % then
    string_codes(ExpectedRest, ActualRest).

test(one_or_max, [
    fail,
    forall(member(Input, [
        "test", "", "alpha test ", "test test test "
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_max(2, "test "), Codes, _)
    % then fails
    . 

test(one_or_max_count, [
    nondet,
    forall(member(case(Input, ExpectedCount, ExpectedRest ), [
        case("test ", 1, ""),
        case("test test ", 2,  ""),
        case("test test alpha", 2,  "alpha"),
        case("test test alpha test ", 2, "alpha test ")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_max_count(2, "test ", ActualCount), Codes, ActualRest),
    % then
    are_equal(ExpectedCount, ActualCount),
    string_codes(ExpectedRest, ActualRest).

test(one_or_max_count, [
    nondet,
    forall(member(case(Input, ExpectedCount, ExpectedRest ), [
        case("test ", 1, ""),
        case("test test ", 2,  ""),
        case("test test alpha", 2,  "alpha"),
        case("test test alpha test ", 2, "alpha test ")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_max_count(2, "test ", ExpectedCount), Codes, ActualRest),
    % then
    string_codes(ExpectedRest, ActualRest).

test(one_or_max_count, [
    fail,
    forall(member(Input, [
        "test ", "", "alpha test ", "test test test "
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:one_or_max_count(3, "test ", 2), Codes, _)
    % then fails
    . 

test(zero_or_more, [
    nondet,
    forall(member(case(Input, ExpectedRest ), [
        case("test ", ""),
        case("test test ", ""),
        case("test test test ", ""),
        case("test test alpha", "alpha"),
        case("test test alpha test ", "alpha test "),
        case("alpha test ", "alpha test "),
        case("alpha", "alpha"),
        case("", "")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:zero_or_more("test "), Codes, ActualRest),
    % then
    string_codes(ExpectedRest, ActualRest).

test(zero_or_max, [
    nondet,
    forall(member(case(Input, ExpectedRest ), [
        case("test ", ""),
        case("test test ", ""),
        case("test test alpha", "alpha"),
        case("alpha test ", "alpha test ")
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:zero_or_max(2, "test "), Codes, ActualRest),
    % then
    string_codes(ExpectedRest, ActualRest).

test(zero_or_max, [
    fail,
    forall(member(Input, [
        "test test test ",
        "test test test alpha"
]))]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(wildcards:zero_or_max(2, "test "), Codes, _)
    % then fails
    . 

:- end_tests(wildcards).