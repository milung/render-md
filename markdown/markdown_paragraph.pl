:- module(markdown_paragraph, []).

paragraph(Context, Html) -->
    inline(Context, Html),
    line_ending,
    end_of_paragraph(Context),
    !.

end_of_paragraph(Context, []) -->
    whites,
    line_ending.

end_of_paragraph(Context, []) -->
    line_ending.
