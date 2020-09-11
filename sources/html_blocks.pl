:- module(html_blocks, []).
%! <module> DCG predicates for matching commonmarks HTML Blocks

:- use_module(library(dcg/basics)).
:- use_module(source(commonmark/line)).
:- use_module(source(commonmark/wildcards)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%


%! atx_heading(+Opening:list, -HeadingBlock:term) is nondet
%  matches [ATX heading](https://spec.commonmark.org/0.29/#atx-headings) block in the stream
html_block(Opening, html_block(Lines) -->
    zero_or_max(3, space),
    html_opening(Opening, Case),
    html_body(Opening, Case, Lines).
    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

html_opening(Opening, 1), ScriptTag, [X] -->
    { member( ScriptTag, [ "<script", "<pre", "<style" ]) },
    ScriptTag, 
    [X],
    { memberchk( X, [ 0'>, 0'\r, 0'\n, 0' , 0'\t ] ) }.
