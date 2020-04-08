:- begin_tests(leaf_blocks).
:- use_module(source(commonmark/test_utils)).
:- use_module(source(commonmark/leaf_blocks)).

:- encoding(utf8).

test( atx_heading, [
    nondet,
    forall(member(case(Input, ExpectedLevel, ExpectedLines, ExpectedTail), [
        case("# foo\n", h1, "foo", ""),
        case("# foo", h1, "foo", ""),
        case("## foo\n\n", h2, "foo", "\n"),
        case("##\nfoo\n\n", h2, "foo", "\n"),
        case("##\n   foo\n\n", h2, "foo", "\n"),
        case("### foo\n\n", h3, "foo", "\n"),
        case("#### foo\n\n", h4, "foo", "\n"),
        case("##### foo\n\n", h5, "foo", "\n"),
        case("###### foo\n\n", h6, "foo", "\n"),
        case(" ## foo\n\n", h2, "foo", "\n"),
        case("   ## foo\n\n", h2, "foo", "\n"),
        case("  ## foo  ## \n\n", h2, "foo", "\n"),
        case("## foo  ## \n\n", h2, "foo", "\n"),
        case("## foo  ##\n\n", h2, "foo", "\n"),
        case("## foo  ## b\n\n", h2, "foo  ## b", "\n"),
        case("## foo##\n\n", h2, "foo##", "\n"),
        case("## foo \\##\n\n", h2, "foo \\##", "\n"),
        case("## foo #\\##\n\n", h2, "foo #\\##", "\n"),
        case("## \n\n", h2, "", "\n"),
        case("#\n\n", h1, "", "\n"),
        case("### ###\n\n", h3, "", "\n")
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:atx_heading([], heading(ActualLevel, ActCodeLines)), Input, ActualTail),
    %then
    ExpectedLevel = ActualLevel,
    flatten(ActCodeLines, FlatLines),
    string_codes(ActualLines, FlatLines),
    are_atomic_equal(ExpectedLines, ActualLines),
    are_atomic_equal(ExpectedTail, ActualTail).

test( atx_heading, [
    fail,
    forall(member(Input, [
        "####### Foo\n\n",
        "#5 Foo\n\n",
        "#hashtag\n\n",
        "\\## Foo",
        "    ## foo\n\n"

    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:atx_heading([],_), Input, _).
    %then - fail. 

test( paragraph, [
    nondet,
    forall(member(case(Input, Expected, ExpectedTail), [
        case("some line 1\r\nsome line 2\r\n\r\n",  "some line 1some line 2", "\r\n"),
        case("some line ",  "some line ", "")        
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:paragraph([], paragraph(Lines)), Input, ActualTail),
    %then
    flatten(Lines, FlatLines),
    string_codes(Actual, FlatLines),
    are_atomic_equal(ExpectedTail, ActualTail),
    are_atomic_equal(Expected, Actual).

test( setext_heading, [
    nondet,
    forall(member(case(Input, ExpectedLevel, ExpectedLines, ExpectedTail), [
        case("Foo *bar*\n=========\n\n", h1, "Foo *bar*", "\n"),
        case("Foo *bar*\n---------\n\n", h2, "Foo *bar*", "\n"),
        case("Foo *bar*\n---------", h2, "Foo *bar*", ""),
        case("Foo *bar*\n---------\n", h2, "Foo *bar*", ""),
        case("  Foo *bar\nbaz*\n====\n", h1, "Foo *barbaz*", ""),
        case("Foo\n-------------------------\n\nnext",h2,"Foo","\nnext"),
        case("Foo\n=\n\nnext",h1,"Foo","\nnext"),
        case("Foo\n=\n\nnext",h1,"Foo","\nnext"),
        case("   Foo\n=\n\nnext",h1,"Foo","\nnext"),
        case("Foo\n   ---       \n\nnext",h2,"Foo","\nnext"),
        case("Foo\\\n---\n\nnext",h2,"Foo\\","\nnext"),
        case("`Foo\n---\n`\nnext",h2,"`Foo","`\nnext"),
        case("`Foo\n---\nnext",h2,"`Foo","next")
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:setext_heading([], heading(ActualLevel, ActCodeLines)), Input, ActualTail),
    %then
    ExpectedLevel = ActualLevel,
    flatten(ActCodeLines, FlatLines),
    string_codes(ActualLines, FlatLines),
    are_atomic_equal(ExpectedLines, ActualLines),
    are_atomic_equal(ExpectedTail, ActualTail).

test( setext_heading, [
    fail,
    forall(member(Input, [
        "Foo *bar*\n-=========\n\n",
        "Foo *bar*\n    ---------\n\n",
        "---------\n\n",
        "=====\n\n",
        "=====\n=====\n\n"
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:setext_heading([],_), Input, _).
    %then - fail. 


test( thematic_break, [
    nondet,
    forall(member(case(Input, ExpectedTail), [
        case("***\n", ""),
        case("---\n\n", "\n"),
        case("___", ""),
        case(" ---\n\n", "\n"),
        case("   ___\n", ""),
        case("----------------------------\n\n", "\n"),
        case("- - -\n\n", "\n"),
        case("* **  ** * **  ** *\n", ""),
        case("-        -         -\n\n", "\n"),
        case("- - -   \n\n", "\n"),
        case("___    \t \n\n", "\n")
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:thematic_break([], _), Input, ActualTail),
    %then    
    are_atomic_equal(ExpectedTail, ActualTail).

test( thematic_break, [
    fail,
    forall(member(Input, [
        "+++\n\n",            
        "===",
        "**",
        "_",
        "--",
        "    ----\n",
        "--- a ---\n\n",  
        "--*--*--\n\n",
        "\t----\n"
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(leaf_blocks:thematic_break([],_), Input, _).
    %then - fail. 
    
:- end_tests(leaf_blocks).