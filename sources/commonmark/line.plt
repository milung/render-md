:- begin_tests(line).
:- use_module(source(commonmark/line)).

:- encoding(utf8).

test( empty_line, [
    nondet,
    forall(member(case(Input, Rest), [
        case("\r\n",  ""),
        case("\ntest this",  "test this"),
        case("  \t \r\nrest", "rest"),        
        case("  \t ", "")
    ]))
]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(line:empty_line, Codes, RestC),
    %then
    string_codes(Rest, RestC).

test( empty_line, [
    fail,
    forall(member(Text, [
        "test\n"," x \r\n", " \t x \r\n string"])) 
]) :-
    % given
    string_codes(Text, Codes),
    % when
    phrase(line:empty_line, Codes, _).
    %then fails   

test( eol, [
    forall(member(Text, ["\ntest","\r\ntest"]))
]) :-
    % given
    string_codes(Text, Codes),
    % when
    phrase(line:eol, Codes, Rest),
    %then
    string_codes("test", Rest).

test( eol, [
    fail,
    forall(member(Text, ["test"," \r\ntest"]))
]) :-
    % given
    string_codes(Text, Codes),
    % when
    phrase(line:eol, Codes, _).
    %then fails

test( inline_sequence, [
    nondet,
    forall(member(case(Input, Line, Rest), [
        case("test this\r\n", "test this", "\r\n"),
        case("test this\n" , "test this", "\n"),
        case("test this" , "test this", ""),
        case("  test this\r\nrest", "  test this", "\r\nrest"),
        case("  test this  \r\n rest", "  test this  ", "\r\n rest"),
        case("\n" , "", "\n"),
        case("" , "", "")
    ]))
]) :-
    % given
    string_codes(Input, Codes),
    string_codes(Rest, RestCodes),
    % when
    phrase(line:inline_sequence(LineC), Codes, RestCodes),
    %then
    string_codes(Line, LineC).

test( nonempty_line, [
    nondet,
    forall(member(case(Input, Line, Rest), [
        case("test this\r\n", "test this", ""),
        case("test this\n" , "test this", ""),
        case("  test this\r\nrest", "test this", "rest"),
        case("  test this  \r\n rest", "test this  ", " rest")
    ]))
]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(line:nonempty_line([], LineC), Codes, RestC),
    %then
    string_codes(Line, LineC),
    string_codes(Rest, RestC).

test( nonempty_line, [
    fail,
    forall(member(Text, [
        "\ntest"," \r\n", " \t  \n "])) 
]) :-
    % given
    string_codes(Text, Codes),
    % when
    phrase(line:nonempty_line([], _), Codes, _).
    %then fails   

test( rest_of_line, [
    nondet,
    forall(member(case(Input, Line, Rest), [
        case("test this\r\n", "test this", ""),
        case("test this\n" > "test this", ""),
        case("  test this\r\nrest", "  test this", "rest"),
        case("  test this", "  test this", "")
    ]))
]) :-
    % given
    string_codes(Input, Codes),
    % when
    phrase(line:rest_of_line(LineC), Codes, RestC),
    %then
    string_codes(Line, LineC),
    string_codes(Rest, RestC).
:- end_tests(line).