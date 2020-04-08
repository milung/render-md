:- module(line, [    
    empty_line//0,
    eol//0,
    inline_sequence//1, % -Codes:codes
    nonempty_line//2,   % +Openings:list, -Line:codes
    rest_of_line//1     % -Rest:codes
    ]).
%! <module> line doing some cool things
%  Predicates for line ...

:- encoding(utf8).

:- use_module(library(dcg/basics)).


%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! empty_line is semidet
% takes empty (with whitespaces) line from the input stream
empty_line --> whites, eol.

%! eol is semidet
%  Succeeds if the next characters in stream represents end of line 
eol --> "\r\n", !.
eol --> "\n", !.
eol --> eos, !.

%! inline_sequence(-Codes:codes) is nondet
%  Lazily matches arbitrary sequence of characters that does not 
%  spans more than single line. Eventual end of line is left in the
%  stream. 
inline_sequence([]) --> [].
inline_sequence([C|String]) -->
    \+ eol,
    [C],
    inline_sequence(String).

%! nonempty_line(+Openings:list, -Line:codes) is semidet
%  Takes the line content after skipping unifying Openings in the stream
nonempty_line(_, Line) -->
    whites, \+ white, \+ eol, rest_of_line(Line).

%! rest_of_line(-Rest:codes) is semdet
% takes any sequence of characters until end of line. 
% End of line is taken from the stream
rest_of_line([C|Rest]) -->
    \+ eol, 
    [C],
    rest_of_line(Rest).
rest_of_line([]) --> eol.


    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%