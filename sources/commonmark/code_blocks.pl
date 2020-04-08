:- module(code_blocks, [
    indented_code//2,   % +Opening:list, -HeadingBlock:term
    fenced_code//2      % +Opening:list, -CodeBlock:term
]).
%! <module> code_blocks doing some cool things
%  Predicates for code_blocks ...

:- encoding(utf8).
:- use_module(library(dcg/basics)).
:- use_module(source(commonmark/line)).
:- use_module(source(commonmark/wildcards)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%
%! fenced_code(+Opening:list, -CodeBlock:term) is nondet) is semidet
%  Matches the occurence of the [fenced code block](https://spec.commonmark.org/0.29/#fenced-code-blocks)
%  in the input stream or fails.
fenced_code(Opening, code(Info, Lines) ) -->
    fenced_code_type( [ 0'`] , Opening, Info, Lines ),
    !.
fenced_code(Opening, code(Info, Lines) ) -->
    fenced_code_type( [ 0'~ ], Opening, Info, Lines ),
    !.

%! indented_code(+Opening:list, -CodeBlock:term) is semidet
%  matches [indented code blocks](https://spec.commonmark.org/0.29/#indented-code-blocks) block in the stream
indented_code(Opening, code('', [Line | Lines])) --> 
    "    ", % code indent
    rest_of_line(Line),
    indented_code_tail(Opening, Lines),
    !.
    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%
fenced_body(_, _, [] ) --> eos, !.
fenced_body(_, _, [] ) --> \+eol, [].
fenced_body(Opening, IndentSize, [Line | Lines] ) -->
    count_or_less(IndentSize, space),
    rest_of_line(Line),
    fenced_body(Opening, IndentSize, Lines ).

fenced_closing(_, FenceChar, FenceLength) -->
    at_most(3, space),
    at_least(FenceLength, FenceChar),
    whites, 
    eol.
fenced_closing(_, _, _) -->
    whites, eos.

fenced_code_type(FenceChar, Opening, Info, Lines ) -->
    fenced_opening(Opening, FenceChar, Info, FenceLength, IndentSize),
    fenced_body(Opening, IndentSize, Lines ),
    fenced_closing(Opening, FenceChar, FenceLength).

fenced_opening(_, FenceChar, Info, FenceLength, IndentSize ) --> 
    at_most_count(3, space, IndentSize),
    at_least_count(3, FenceChar, FenceLength),
    \+ FenceChar,
    whites, 
    inline_sequence(InfoCodes),
    whites, 
    eol,     
    {
        (
            FenceChar == [ 0'~ ]
        ;
            \+ member(0'`, InfoCodes)
        ),
        atom_codes(Info, InfoCodes)
    },
    !.
    
    
indented_code_tail(Opening, [Line | Lines]) --> 
    "    ", % code indent
    rest_of_line(Line),
    indented_code_tail(Opening, Lines),
    !.
indented_code_tail(Opening, [ [] | Lines]) -->     
    \+ eos,
    zero_or_max(3, space), % code indent
    eol,
    indented_code_tail(Opening, Lines),
    !.
indented_code_tail(_ , []) --> 
    [].
