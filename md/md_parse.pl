:- module(md_parse, [
    md_parse_codes/2,  % +Codes, -Blocks
    md_parse_stream/2, % +Stream, -Blocks
    md_parse_file/2,   % +File, -Blocks
    md_parse_string/2, % +String, -Blocks
    md_html_codes/2,   % +Codes, -HtmlString
    md_html_stream/2,  % +Stream, -HtmlString
    md_html_file/2,    % +File, -HtmlString
    md_html_string/2,  % +String, -HtmlString
    md_parse_book/3    % +String, -Blocks
]).

/** <module> Prolog Markdown parser

Top-level module for parsing Markdown. Contains
some convenience predicated.
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).

:- use_module('md/md_links').
:- use_module('md/md_blocks').
:- use_module('md/md_book').

%! md_parse_string(+String, -Blocks) is det.
%
% Same as md_parse_codes/2 but takes
% a string instead.

md_parse_string(String, Blocks):-
    string_codes(String, Codes),
    md_parse_codes(Codes, Blocks).

%! md_parse_codes(+Codes, -Blocks) is det.
%
% Parses Markdown into a structure suitable in use
% with html//1.

md_parse_codes(Codes, Blocks):-
    md_links(Codes, Tmp),
    phrase(md_blocks(Out), Tmp), !,
    Blocks = Out.

%! md_parse_stream(+Stream, -Blocks) is det.
%
% Same as md_parse_codes/2 but reads input from stream.

md_parse_stream(Stream, Blocks):-
    read_stream_to_codes(Stream, Codes),
    md_parse_codes(Codes, Blocks).

%! md_parse_file(+Name, -Blocks) is det.
%
% Same as md_parse_codes/2 but reads input from file.

%! md_parse_file(+Name, -Blocks) is det.
%
% Same as md_parse_file(Name, Blocks, [encoding(utf8)]) .

md_parse_file(File, Blocks):-
    md_parse_file(File, Blocks, [encoding(utf8)]).

%! md_parse_file(+Name, -Blocks) is det.
%
% Same as md_parse_codes/2 but reads input from file. Options are handed over to read_file_to_codes/3.

md_parse_file(File, Blocks, Options):-
    read_file_to_codes(File, Codes, Options),
    md_parse_codes(Codes, Blocks).

%! md_html_codes(+Codes, -Html) is det.
%
% Converts Markdown into HTML string.

md_html_codes(Codes, Html):-
    md_parse_codes(Codes, Blocks),
    phrase(html(Blocks), Tokens),
    with_output_to(string(Html), print_html(Tokens)).

%! md_html_string(+String, -Html) is det.
%
% Same as md_html_codes/2 but takes
% input as string.

md_html_string(String, Html):-
    string_codes(String, Codes),
    md_html_codes(Codes, Html).

%! md_html_stream(+Stream, -Html) is det.
%
% Same as md_html_codes/2 but reads input from stream.

md_html_stream(Stream, Html):-
    read_stream_to_codes(Stream, Codes),
    md_html_codes(Codes, Html).

%! md_html_file(+Name, -Html) is det.
%
% Same as md_html_codes/2 but reads input from file.

md_html_file(File, Html):-
    read_file_to_codes(File, Codes, []),
    md_html_codes(Codes, Html).

%! md_parse_book(+ManifestFile, -Html) is det.
%
% Loads GitBook manifest and parses the pook into html blocks.

md_parse_book(RootDirectory, Section, Html) :-    
    directory_file_path(RootDirectory, 'book.json', ManifestFile),
    open(ManifestFile, read, Stream),
    json_read_dict(Stream, BookDict),        
    md_book(BookDict.put(base,RootDirectory), Section, Html),
    close(Stream).