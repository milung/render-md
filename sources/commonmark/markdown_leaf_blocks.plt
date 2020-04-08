:- begin_tests(markdown_leaf_blocks).
:- use_module('markdown/markdown_leaf_blocks').

test(thematic_break, [nondet]) :-
    string_codes("***  \n \n lorem", Codes),
    string_codes(" \n lorem", ExpectedCodes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, ActualCodes ),
    ActualCodes = ExpectedCodes.

test(thematic_break, [nondet]) :-
    string_codes("---\n \n lorem", Codes),
    string_codes(" \n lorem", ExpectedCodes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, ActualCodes ),
    ActualCodes = ExpectedCodes.

test(thematic_break, [nondet]) :-
    string_codes("  ___  \n \n lorem", Codes),
    string_codes(" \n lorem", ExpectedCodes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, ActualCodes ),
    ActualCodes = ExpectedCodes.

test(thematic_break, [nondet]) :-
    string_codes("  _ _   _ \n \n lorem", Codes),
    string_codes(" \n lorem", ExpectedCodes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, ActualCodes ),
    ActualCodes = ExpectedCodes.

test(thematic_break, [nondet]) :-
    string_codes("** **** * ** **\n \n lorem", Codes),
    string_codes(" \n lorem", ExpectedCodes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, ActualCodes ),
    ActualCodes = ExpectedCodes.

test(thematic_break, [fail]) :-
    string_codes("+++  \n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(thematic_break, [fail]) :-
    string_codes("===  \n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(thematic_break, [fail]) :-
    string_codes("     ***  \n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(thematic_break, [fail]) :-
    string_codes("*-*  \n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(thematic_break, [fail]) :-
    string_codes("*** foo  \n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(thematic_break, [fail]) :-
    string_codes("---a---\n \n lorem", Codes),
    phrase(markdown_leaf_blocks:thematic_break, Codes, _ ).

test(atx_heading) :-
    string_codes("# foo\nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h1([a([id(foo), class(anchor), aria-hidden(true)], []),\["foo"]]).

test(atx_heading) :-
    string_codes("# \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h1([]).

test(atx_heading) :-
    string_codes("###### foo  \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h6([a([id(foo), class(anchor), aria-hidden(true)], []),\["foo"]]).

test(atx_heading) :-
    string_codes("##\n  foo  \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h2([a([id(foo), class(anchor), aria-hidden(true)], []),\["foo"]]).     

test(atx_heading) :-
    string_codes("  ### foo bar \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar'),class(anchor),aria-hidden(true)],[]),\["foo bar"]]) .

test(atx_heading, [ ]) :-
    string_codes("  ###   foo bar    ###### \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar'),class(anchor),aria-hidden(true)],[]),\["foo bar"]]).

test(atx_heading, [ ]) :-
    string_codes("  ###   foo bar### \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar%23%23%23'),class(anchor),aria-hidden(true)],[]),\["foo bar###"]]).

test(atx_heading, [ ]) :-
    string_codes("  ###   foo bar ## ### \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar-%23%23'),class(anchor),aria-hidden(true)],[]),\["foo bar ##"]]).

test(atx_heading, [ ]) :-
    string_codes("  ###   foo bar    \\### \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar-%23%23%23'),class(anchor),aria-hidden(true)],[]),\["foo bar ###"]]).

test(atx_heading, [ ]) :-
    string_codes("  ###   foo bar    ##\\# \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-bar-%23%23%23'),class(anchor),aria-hidden(true)],[]),\["foo bar ###"]]).
    
test(atx_heading, [ ]) :-
    string_codes("  ###   foo ###  bar    ###### \nlorem", Codes),
    string_codes("lorem", ExpectedRest),
    phrase(markdown_leaf_blocks:atx_heading(Html), Codes, ActualRest),
    ActualRest = ExpectedRest,
    Html = h3([a([id('foo-%23%23%23-bar'),class(anchor),aria-hidden(true)],[]),\["foo ### bar"]]).

test(atx_heading, [ fail ]) :-
    string_codes("#5 BOLT \nlorem", Codes),
    phrase(markdown_leaf_blocks:atx_heading(_), Codes, _).

test(atx_heading, [ fail ]) :-
    string_codes("\\### BOLT \nlorem", Codes),
    phrase(markdown_leaf_blocks:atx_heading(_), Codes, _).

:- end_tests(markdown_leaf_blocks).