:- module(leaf_blocks, [
    atx_heading//2,     % +Opening:list, -HeadingBlock:term
    paragraph//2,       % +Opening:list, -HeadingBlock:term
    setext_heading//2   % +Opening:list, -HeadingBlock:term
]).
%! <module> DCG predicates for matching commonmark leaf blocks. 
% Code leaf blocks are placed in another module

:- encoding(utf8).

:- use_module(library(dcg/basics)).
:- use_module(source(commonmark/line)).
:- use_module(source(commonmark/wildcards)).

%! atx_heading(+Opening:list, -HeadingBlock:term) is nondet
%  matches [ATX heading](https://spec.commonmark.org/0.29/#atx-headings) block in the stream
atx_heading(Opening, heading(Level, [])) -->
    zero_or_max(3, space),
    atx_opening(Opening, Level),            
    atx_closing,    
    eol.
atx_heading(Opening, heading(Level, Line)) -->
    zero_or_max(3, space),
    atx_opening(Opening, Level),    
    white, whites, 
    inline_sequence(Line),
    {
        \+ atom_length(Line, 0)
    },
    atx_closing,    
    eol.

%! paragraph(+Opening:list, -Block:term) is nondet
%  Matches [paragraph](https://spec.commonmark.org/0.29/#paragraphs) block in the stream
paragraph(Opens, paragraph([Line])) -->
    nonempty_line(Opens, Line).

paragraph(Opens, paragraph([Line|Lines])) -->
    nonempty_line(Opens, Line),
    paragraph(Opens, paragraph(Lines)).

%! setext_heading(+Opening:list, -Block:term) is nondet
%  Matches [setext](https://spec.commonmark.org/0.29/#setext-headings) block in the stream
setext_heading(Opens, heading(Level, [Line|Lines])) -->
    \+ setext_underline(Opens, Level),
    nonempty_line(Opens, Line),
    setext_heading(Opens, heading(Level, Lines)).

setext_heading(Opens, heading(Level, [Line])) -->
    \+ setext_underline(Opens, Level),
    nonempty_line(Opens, Line),    
    setext_underline(Opens, Level).

thematic_break(Opening, hr) --> 
    thematic_break_type(Opening, 0'*).
thematic_break(Opening, hr) --> 
    thematic_break_type(Opening, 0'-).
thematic_break(Opening, hr) --> 
    thematic_break_type(Opening, 0'_).

    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

atx_closing -->
    white, whites,
    zero_or_more("#"),
    whites.
atx_closing -->
    whites.

atx_opening(_, Level) -->
    one_or_max_count(6, "#", LevelNum), 
    
    {
        atom_concat(h,LevelNum, Level)
    }.
    
atx_opening(_, Level), [ 32 ] -->
    one_or_max_count(6, "#", LevelNum), 
    whites, eol,
    {
        atom_concat(h,LevelNum, Level)
    }.

setext_underline(_, h1) -->
    zero_or_max(3, space),
    one_or_more("="),
    whites, 
    eol.
setext_underline(_, h2) -->
    zero_or_max(3, space),
    one_or_more("-"),
    whites, 
    eol.

thematic_break_type(_, C) --> 
    zero_or_max(3, space),
    at_least(3, 
        (
            [C],
            whites
        )),
    eol.