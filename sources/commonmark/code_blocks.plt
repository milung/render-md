:- begin_tests(code_blocks).
:- use_module(source(commonmark/test_utils)).
:- use_module(source(commonmark/code_blocks)).

:- encoding(utf8).


test(fenced_code, [
    nondet,
    forall( member( case(Input, Expected, ExpectedInfo, ExpectedTail), [
        case(   "```\n<\n >\n```\n" , "< >", "", ""),
        case(   "~~~\n<\n >\n~~~\n" , "< >", "", ""),
        case(   "```\naaa\n~~~\n```\n", "aaa~~~", "", ""),
        case(   "~~~\naaa\n```\n~~~\n", "aaa```", "", ""),
        case(   "````\naaa\n```\n``````\n", "aaa```", "", ""),
        case(   "~~~~\naaa\n~~~\n~~~~~~\n", "aaa~~~", "", ""),
        case(   "```", "", "", ""),
        case(   "`````\n   \n```\naaa\n", "   ```aaa", "", ""),
        case(   "```\n\n  \n```\n", "  ", "", ""),
        case(   "```\n```\n", "", "", ""),
        case(   " ```\n aaa\n aaa\n ```\n", "aaaaaa", "", ""),
        case(   "  ```\naaa\n  aaa\naaa\n  ```\n", "aaaaaaaaa", "", ""),
        case(   "  ```\n   aaa\n    aaa\n  aaa\n   ```\n", " aaa  aaaaaa", "", ""),
        case(   "```\naaa\n  ```\n", "aaa", "", ""),
        case(   "   ```\naaa\n  ```\n", "aaa", "", ""),
        case(   "```\naaa\n    ```\n", "aaa    ```", "", ""),
        case(   "~~~~~~\naaa\n~~~ ~~\n", "aaa~~~ ~~", "", ""),
        case(   "```ruby\ndef foo(x)\n  return 3\nend\n```\n", "def foo(x)  return 3end", "ruby", "" ),
        case(   "~~~~    ruby startline=3 $%@#$  \ndef foo(x)\n  return 3\nend\n~~~~~~~\n", 
                "def foo(x)  return 3end", "ruby startline=3 $%@#$", ""),
        case(   "````;\n````\n", "", ";", ""),
        case(   "~~~ aa ``` ~~~\nfoo\n~~~\n", "foo", "aa ``` ~~~", ""),
        case(   "```\n``` aaa\n```\n", "``` aaa", "", "")        
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(code_blocks:fenced_code([], code(ActualInfo, Lines)), Input,  ActualTail),
    % then
    %then
    flatten(Lines, FlatLines),
    string_codes(Actual, FlatLines),
    are_atomic_equal(ExpectedTail, ActualTail),
    are_atomic_equal(Expected, Actual),
    are_atomic_equal(ExpectedInfo, ActualInfo).

test(fenced_code, [
    fail,
    forall( member( Input, [
        "``\nfoo\n``\n",
        "    ```\n    aaa\n    ```\n",
        "``` ```\naaa\n",
        "``` aa ```\nfoo\n"
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(code_blocks:fenced_code([], _), Input, _).
    % then - fail
    
test(indented_code, [
    nondet,
    forall( member( case(Input, Expected, ExpectedTail), [
        case([ "    a simple\n","      indented code block\n" ],
                [ "a simple", "  indented code block" ], ""),
        case([ "    <a/>\n", "    *hi*\n", "\n", "    - one\n" ], 
                [ "<a/>", "*hi*", "", "- one" ], ""),
        case([ "    chunk 1\n", "\n", "    chunk 2\n", "  \n", " \n", " \n", "    chunk 3\n" ],
                [ "chunk 1", "", "chunk 2", "", "", "", "chunk 3" ], ""),
        case([ "    chunk 1\n", "       \n", "       chunk 2\n" ],
                [ "chunk 1", "   ", "   chunk 2" ], ""),
        case([ "    chunk 1\n", "  foo\n" ],
                [ "chunk 1" ],  "  foo\n")
    ]))
]) :-
    % given - case
    % when
    phrase_atomic_string(code_blocks:indented_code([], code('', Lines)), Input, ActualTail),
    % then
    %then
    flatten(Lines, FlatLines),
    string_codes(Actual, FlatLines),
    are_atomic_equal(ExpectedTail, ActualTail),
    are_atomic_equal(Expected, Actual).
    
:- end_tests(code_blocks).
