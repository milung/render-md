:- module(markdown_leaf_blocks, [
    markdown_leaf_blocks//1     % -Html - termed html output
    ]).

:- use_module(library(dcg/basics)).
:- use_module(markdown/markdown_basics).
:- use_module(markdown/markdown_inlines).

markdown_leaf_blocks(hr('')) -->
    thematic_break.

markdown_leaf_blocks(Html) -->
    atx_heading(Html).

% thematic_break// is nondet.
%
% Matches [thematic  blocks](https://spec.commonmark.org/0.29/#thematic-break)
%
thematic_break -->
    thematic_break_chars(3, _),
    line_ending. 

% atx_heading(-Html)// is nondet.
%
% Matches [ATX heading](https://spec.commonmark.org/0.29/#atx-heading)
%
atx_heading(Html) -->
    offset(0,3, _),
    atx_opening(Level, 1),
    inline_anchor(HtmlInline, Anchor),
    ( 
        space_char,
        atx_closing(start)
    ;
        line_ending
    ),

    {
        atom_concat(h,Level, Head),
        (
            atom_length(Anchor, 0),
            Html =.. [ Head, [] ]
        ;
            Html =.. [ Head, [
                a([id(Anchor), class(anchor), aria-hidden(true)], []),
                HtmlInline ]
            ]
        )
    },
    !.

% setext_heading(-Html)// is nondet.
%
% Matches [Setext heading](https://spec.commonmark.org/0.29/#setext-heading)
%
setext_heading(Html) -->
    offset(0,3, _),
    inline_anchor(Inline, Anchor),
    line_ending,
    setext_underline.

setext_underline --> 
    offset(0, 3, _), 
    one_or_more(0'-),
    whites, 
    line_ending.


atx_opening(Level, Level) -->
    [ 0'# ],
    space_char,
    whites.
atx_opening(Level, Level) -->
    [ 0'# ],
    line_ending,
    whites.

atx_opening(LevelOut, LevelIn) -->
    [ 0'# ],
    {
        Level1 is LevelIn + 1,
        !,
        Level1 =< 6
    },
    atx_opening(LevelOut, Level1).

atx_closing(_) --> 
    whites,
    line_ending.
atx_closing(start) -->
    whites,
    [ 0'# ],
    atx_closing(end).
atx_closing(_) -->
    [ 0'# ],
    atx_closing(end).


thematic_break_chars(0, Char) -->
    thematic_break_char(Char),
    whites,
    thematic_break_chars(0, Char).
thematic_break_chars(0, Char) -->
    thematic_break_char(Char),
    whites,
    !.

thematic_break_chars(N, Char) -->
    { N > 0, N1 is N -1 },
    space_char,
    thematic_break_chars(N1, Char).

thematic_break_chars(N, Char) -->
    { N > 0 },
    thematic_break_chars(0, Char).

thematic_break_char(Char) --> 
    [ Char ],
    {
        memberchk(Char, [0'- , 0'_ , 0'* ])
    }.
    
