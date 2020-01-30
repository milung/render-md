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
    md_parse_files(Files, Html, BaseUrl).    

md_parse_files(Files, Html, BaseUrl) :-
    md_merge_files(Files, Codes, BaseUrl), 
    flatten(Codes, FlatCodes),    
    md_parse_codes(FlatCodes, Html).

md_merge_files([], [], _).
md_merge_files([File|Files], [CodesFile, Codes], BaseUrl) :-  
    read_file_to_codes(File, CodesFile, [encoding(utf8)]),
    (
        BaseUrl \= false,
        rebase_local_links(BaseUrl, Codes, Codes1)
    ;
        Codes1 = Codes
    ),
    md_merge_files(Files, Codes1, BaseUrl).

rebase_local_links(_, [], []).
rebase_local_links(BaseUrl, CodesIn, CodesOut) :-
    atom_codes('](./', SearchCodes),
    append(SearchCodes, Codes1, CodesIn),
    atom_codes(BaseUrl, Prefix0),
    append(Prefix0, BaseUrl, Prefix),
    append(Prefix, Codes1, Codes2),
    rebase_local_links(BaseUrl, Codes2,CodesOut).
rebase_local_links(BaseUrl, [X|Codes], [X|CodesOut]) :-
    rebase_local_links(BaseUrl, Codes,CodesOut).
