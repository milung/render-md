:- module(md_blocks, [
    md_blocks//1 % -Blocks
]).

/** <module> Block-level parser for Markdown

Parses Markdown block-level constructs like
paragraphs, lists, code blocks, blockquotes etc.
Applies span-level parsing for all blocks.
*/

:- use_module(library(dcg/basics)).

:- use_module(md/md_list_item).
:- use_module(md/md_header).
:- use_module(md/md_span).
:- use_module(md/md_trim).
:- use_module(md/md_line).
:- use_module(md/md_hr).

%! md_blocks(-Blocks)// is det.
%
% Parses given Markdown into a structure
% accepted by html//1.

md_blocks(Blocks) -->
    blocks(top, [], Blocks).

% Contextified block parsing.
% Some types of blocks are not
% allowed in contexts other than top.
% Currently used contexts are: top,
% list and bq.

md_blocks(Ctx, Blocks) -->
    blocks(Ctx, [], Blocks).

% Recognizes all blocks
% in the input. When a block is not
% recognized, one line as removed and
% added into accumulator. These accumulated
% lines are added as paragraph blocks.
% This matches better the sematics of
% http://daringfireball.net/projects/markdown/dingus

blocks(Ctx, Acc, Result) -->
    empty_lines,
    block(Ctx, Block), !,
    {
        (   Acc = []
        ->  Result = [Block|Blocks]
        ;   acc_block(Acc, AccBlock),
            Result = [AccBlock,Block|Blocks])
    },
    blocks(Ctx, [], Blocks).

blocks(Ctx, Acc, Blocks) -->
    non_empty_line(Line), !,
    blocks(Ctx, [Line|Acc], Blocks).

blocks(_, Acc, Result) -->
    empty_lines,
    eos, !,
    {
        (   Acc = []
        ->  Result = []
        ;   Result = [Block],
            acc_block(Acc, Block))
    }.

blocks(Ctx, Acc, Result) -->
    empty_line,
    {
        (   Acc = []
        ->  Result = Blocks
        ;   Result = [Block|Blocks],
            acc_block(Acc, Block))
    },
    blocks(Ctx, [], Blocks).

% Converts lines into a <p>
% element and applies span-level
% parsing.

acc_block(Acc, p(Span)):-
    reverse(Acc, AccLines),
    merge_lines(AccLines, Block),
    md_span_codes(Block, Span).

% Recognizes a single block.
% Tries to parse in the following
% order: headers, horisontal ruler,
% lists, blockquote, html.

block(_, Block) -->
    md_header(Block), !.

% block(_, Block) -->
%     code(Block), !.

block(top, hr([])) -->
    md_hr, !.

block(_, Block) -->
    list(Block), !.

block(top, Block) -->
    blockquote(Block), !.

block(_, Block) -->
    html(Block), !.

block(_, Block) -->
    fenced_code(Block).

block(_, Block) -->
    table(Block).

table(table([Head | tbody(Rows)])) -->
    table_head(Head), 
    table_rows(Rows).

table(table(tbody(Rows))) -->
    table_rows( Rows).
    
table_head( thead(tr(Cells)) ) -->
    table_head_row(Cells),
    hyphens_row(_Alignments). 

table_rows( [tr(Row) | Rows ]) -->
    table_row( Row ),
    table_rows( Rows ).
table_rows( [ tr(Row) ]) -->
    table_row( Row ),
    !.

table_row( [Cell] ) --> 
    whites, 
    table_cell(Cell),
    row_end,
    !.
table_row( [Cell| Cells] ) -->
    whites, 
    table_cell(Cell),
    table_row(Cells).

table_head_row( [th(Cell)] ) --> 
    whites, 
    table_cell(td(Cell)), 
    row_end,
    !.
table_head_row( [ th(Cell) | Cells] ) -->
    whites, 
    table_cell(td(Cell)),
    table_head_row(Cells).

table_cell(td(Html)) --> 
    "|",
    table_content(Content), 
    cell_end,
    { 
        md_span_codes(Content, Html)
    }.

table_content([]), "|" --> ( "|"; "\n" ), !.
table_content([ X | T]) --> 
    [X],
    table_content(T).


row_end --> ( "|"; [] ), whites, "\n".

cell_end, "|" --> whites, "|".
cell_end, "|" --> whites, "\n".

hyphens_row([Alignment]) --> 
    hyphens_cell(Alignment),
    row_end.
hyphens_row([Alignment| Alignments]) --> 
    whites,
    hyphens_cell(Alignment),
    hyphens_row(Alignments). 

hyphens --> "-", hyphens.
hyphens --> !.

hyphens_cell(left) --> "|---", hyphens, cell_end.
hyphens_cell(right), "|" --> "|", " ", whites, hyphens, "---|".
hyphens_cell(midle) --> "|", " ", whites, "---", hyphens, " ", whites, cell_end.

% Recognizes fenced code blocks.
% The language is put into the
% `data-language` attribute of the
% `code` tag.

fenced_code(Block) -->
    indent_start(0, Indent), 
    "```", inline_string(LangCodes), ln,
    indented_lines(Indent, Lines),
    whites, "```", whites, ln_or_eos, !,
    {
        trim(LangCodes, Trimmed),
        atom_codes(Lang, Trimmed),
        merge_lines(Lines, Codes),
        string_codes(Code, Codes),
        (   Lang = ''
        ->  Block = pre(code(Code))
        ;   Block = pre(code(['data-language'=Lang, class=Lang], Code)))
    }.


indent_start(Current, Actual) -->
    white,
    { N1 is Current + 1} ,
    indent_start(N1, Actual).
indent_start(Actual, Actual) --> [].

indent(0) --> [], !.
indent(N) --> 
    white,
    { N1 is N - 1},
    indent(N1).
    
indented_lines(_, []) --> [].
indented_lines(Indent, [Codes|Lines]) -->
    indent(Indent), string(Codes), ln,    
    indented_lines(Indent, Lines).


% Optimizes generated HTML structure.
% Applied after parsing different blocks.
% Mostly deals with excessive <p> elements
% removal.

optimize(blockquote([p(Block)]), blockquote(Block)):- !.

optimize(li([p(Block)]), li(Block)):- !.

optimize(li([p(Block1), ul(Block2)]), li(Block)):- !,
    append(Block1, [ul(Block2)], Block).

optimize(li([p(Block1), ol(Block2)]), li(Block)):- !,
    append(Block1, [ol(Block2)], Block).

optimize(Block, Block).



% Recognizes block-level HTML.
% No Markdown inside it is processed.
% Gives term that write_html's html//1
% does not escape.

html(\[String]) -->
    [0'<, Code], { code_type(Code, alpha) }, !,
    non_empty_lines(Html),
    { string_codes(String, [0'<,Code|Html]) }.

% Recognizes either ordered list
% or bulleted list.

list(List) -->
    bullet_list(List), !.

list(List) -->
    ordered_list(List).

% Recognizes ordered list.
% Gives term like ol(Term)
% where Items is non-empty list.

ordered_list(ol(Items)) -->
    ordered_list_collect(Items, _), !,
    { Items \= [] }.

ordered_list_collect([Item|Items], Mode) -->
    ordered_list_item(Item, Mode), !,
    empty_lines,
    ordered_list_collect(Items, Mode).

ordered_list_collect([], _) --> "".

% Recognizes a single ordered list item.

ordered_list_item(Item, ListMode) -->
    md_ordered_list_item(Codes, ItemMode),
    { postproc_list_item(Codes, ItemMode, ListMode, Item) }.

% Recognizes bulleted list.
% Gives a term like ul(Items)
% where Items is non-empty list.

bullet_list(ul(Items)) -->
    bullet_list_collect(Items, _), !,
    { Items \= [] }.

bullet_list_collect([Item|Items], Mode) -->
    bullet_list_item(Item, Mode), !,
    empty_lines,
    bullet_list_collect(Items, Mode).

bullet_list_collect([], _) --> "".

% Recognizes a single bulleted list item.

bullet_list_item(Item, ListMode) -->
    md_bullet_list_item(Codes, ItemMode),
    { postproc_list_item(Codes, ItemMode, ListMode, Item) }.

% Postprocesses a list item.
% In paragraph mode, no optimizations are
% applied (preserves `<p>` in `<li>`).
% The actual list mode is set by the first
% list item.

postproc_list_item(Codes, ItemMode, ListMode, Item):-
    phrase(md_blocks(list, Blocks), Codes),
    list_mode(ListMode, ItemMode, Mode),
    (   Mode = normal
    ->  optimize(li(Blocks), Item)
    ;   Item = li(Blocks)).

% List mode setup. When ListMode
% is set, its value is used. Otherwise
% ListMode is set to ItemMode.

list_mode(ListMode, ItemMode, Mode):-
    (   var(ListMode)
    ->  ListMode = ItemMode
    ;   true),
    Mode = ListMode.

% Recognizes a blockquote.
% Strips > from line beginnings.
% Output is a term like blockquote(Blocks).

blockquote(Opt) -->
    ">", string(Codes),
    empty_line,
    empty_line, !,
    {
        trim_left(Codes, Trimmed),
        phrase(bq_strip(Stripped), Trimmed), !,
        phrase(md_blocks(top, Blocks), Stripped),
        optimize(blockquote(Blocks), Opt)
    }.

% Strips > from blockquote line
% beginnings.

bq_strip([0'\n|Codes]) -->
    ln, "> ", !, bq_strip(Codes).

bq_strip([0'\n|Codes]) -->
    ln, ">", !, bq_strip(Codes).

bq_strip([Code|Codes]) -->
    [Code], !, bq_strip(Codes).

bq_strip([]) -->
    eos.

% List of consequtive non-empty lines.
% Consumes as many non-empty lines
% as possible. Gives flattened list
% of codes.

non_empty_lines(Codes) -->
    non_empty_lines_collect(Lines),
    { merge_lines(Lines, Codes) }, !.

non_empty_lines_collect([Line|Lines]) -->
    non_empty_line(Line), !,
    non_empty_lines_collect(Lines).

non_empty_lines_collect([]) --> "".
