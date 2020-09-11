:- module(blocks, []).

:- use_module(library(dcg/basics)).
:- use_module(source(commonmark/line)).
:- use_module(source(commonmark/wildcards)).

document(Blocks) --> 
    blocks([], Blocks).

blocks(Opens, [Block | Blocks]) -->
    block(Opens, Block),
    blocks(Opens,  Blocks).
blocks(Opens, [Block | Blocks]) -->
    block(Opens, Block),
    empty_line,
    blocks(Opens,  Blocks).
blocks(_, []) --> [].

block(Opens, Block) --> 
    setext_heading(Opens, Block).
block(Opens, Block) --> 
    paragraph(Opens, Block).







%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%




%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

