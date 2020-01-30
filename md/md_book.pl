:- module( md_book, [md_book/3]).

:- use_module('md/md_parse').

md_book(ManifestDict, SectionFile, Html):- 
    directory_file_path(ManifestDict.base, ManifestDict.'source-root', RootPath),
    access_file(SectionFile, read), 
    directory_file_path(RootPath, ManifestDict.links, LinksFile),
    (
        access_file(LinksFile, read), 
        Files = [SectionFile, LinksFile ]
    ;
        Files = [SectionFile]
    ),
    md_parse_files(Files, Html).    

md_parse_files(Files, Html) :-
    md_merge_files(Files, Codes), 
    flatten(Codes, FlatCodes),    
    md_parse_codes(FlatCodes, Html).

md_merge_files([], []).
md_merge_files([File|Files], [CodesFile, Codes]) :-  
    read_file_to_codes(File, CodesFile, [encoding(utf8)]),  
    md_merge_files(Files, Codes).