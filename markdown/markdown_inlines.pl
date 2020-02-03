:- module(markdown_inlines, [
    inline//1,           % -Html
    inline_anchor//2    % -Html, -Fragment
    ]).

:- use_module(library(dcg/basics)).

inline( Html ) --> inline_anchor(Html, _).

inline_anchor(\[Text], Anchor) --> 
    inline_codes(Codes, AnchorCodes),
    {
        string_codes( Text, Codes),
        string_codes( AnchorText, AnchorCodes),
        uri_encoded(fragment, AnchorText, Anchor)
    }.

inline_codes([], []) --> [].

inline_codes( [ 0'  | Codes],  Anchor) -->
    white,
    inline_codes( [ S | Codes ], Anchor),
    { code_type(S, space) }.

inline_codes( [ 0' , S |  Codes], [ 0'- | Anchor]) -->
    white,
    inline_codes( [ S | Codes ],  Anchor),
    { \+ code_type(S, space) }.

inline_codes( [C | Codes], [C | Anchor]) -->
    [ 0'\\, C ],  { C \= 0'\n, C \= 0'\r},
    inline_codes( Codes, Anchor ).
inline_codes( [C | Codes], [C | Anchor]) -->
    [ C ],  { C \= 0'\n, C \= 0'\r},
    inline_codes( Codes, Anchor ).
    
    
    
    