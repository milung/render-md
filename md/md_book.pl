:- module( md_book, [md_book/3, md_book/4]).

:- use_module('md/md_parse').

md_book(ManifestDict, SectionFile, Html) :- 
    md_book(ManifestDict, SectionFile, Html, false).

md_book(ManifestDict, SectionFile, Html, BaseUrl):- 
    directory_file_path(ManifestDict.base, ManifestDict.'source-root', RootPath),
    access_file(SectionFile, read), 
    directory_file_path(RootPath, ManifestDict.links, LinksFile),
    (
        access_file(LinksFile, read), 
        Files = [SectionFile, LinksFile ]
    ;
        Files = [SectionFile]
    ),
    base_normalized(BaseUrl, NormalizedBase),
    md_parse_files(Files, Html, NormalizedBase).    

base_normalized('/', '/') :- !.
base_normalized('', '/') :- !.
base_normalized(Base, Base) :- 
    atom_concat(_, '/', Base),
    !.
base_normalized(Base, Normal) :- 
    atom_concat(Base, '/', Normal).

md_parse_files(Files, Html, BaseUrl) :-
    md_merge_files(Files, Codes, BaseUrl), 
    flatten(Codes, FlatCodes),    
    md_parse_codes(FlatCodes, Html).

md_merge_files([], [], _).
md_merge_files([File|Files], [CodesFile, CodesOut], BaseUrl) :-  
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    (
        BaseUrl \= false,
        rebase_local_links(BaseUrl, Codes, CodesFile)
    ;
        CodesFile = Codes
    ),
    md_merge_files(Files, CodesOut, BaseUrl).

rebase_local_links(_, [], []).
rebase_local_links(BaseUrl, [ 0'], 0'(, 0'., 0'/ |CodesIn], CodesOut) :-
    atom_codes(BaseUrl, Prefix0),    
    append([ 0'], 0'( ], Prefix0, Prefix),
    append( Prefix, CodesIn, Codes2),
    rebase_local_links(BaseUrl, Codes2, CodesOut).
rebase_local_links(BaseUrl, [X|Codes], [X|CodesOut]) :-
    rebase_local_links(BaseUrl, Codes,CodesOut).
