
:- use_module(library(dcg/basics)).

generate_full_doc :-
    setup_call_cleanup(
        open("FULL.md",write, Stream, [encodign(utf8)]),
        phrase_from_file(full_doc(Stream), "book-src/SUMMARY.md"),
        close(Stream)).

full_doc(Stream) -->
    {        
        writeln(Stream, '[TOC]'),
        nl(Stream),
        nl(Stream)
    },
    summary(Stream).

summary(_) -->
    eos,
    !.
summary(Stream) -->
    "](",                
    string_without(")",Link),
    ")",
    {
        atom_codes(LinkAtom, Link),
        atomic_list_concat([ 'book-src/', LinkAtom ], Path),            
        read_file_to_string(Path, String,  [ encodign(utf8) ]),
        writeln(Stream, String),
        nl(Stream)
    },
    !,
    summary(Stream).
summary(Stream) -->
    [_],                
    summary(Stream).


