:- module(md_manifest, [
    load_manifest/2,
    load_documentation_manifest/2
]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).

:- use_module(md/md_book).

:- setting(book_base, atom, env(book_base, '/var/www/book'), 'Book root directory (ENV book_base)').

% public
Manifest.asset_path(RelativePath) := book(FilePath) :-      
    directory_file_path(Manifest.'source-root', RelativePath, FilePath).

%oublic 
Manifest.render_body(BaseUrl, Section) := Html :-    
    md_book(Manifest, Section, SectionHtml),    
    SummaryPath = Manifest.resolve_section_path(Manifest.summary),
    md_book(Manifest, SummaryPath, SummaryHtml, BaseUrl),
    Html = div(class='page-layout', [
        div(class='book-section',[
            div(class='padding-left',''),
            div(class='section-body', SectionHtml),
            div(class='padding-right','')]), 
        div(class='book-summary',SummaryHtml),
        div([id(modal), class(modal)], [
            span(class('modal-close'), x ),
            img([class('modal-content'), id('modal-img')]),
            div(id('modal-caption'), '')]), 
        script([src='/_themes/modal.js'], '')]).

Manifest.render_body(_BaseUrl, Section) := Html :-    
    md_book(Manifest, Section, SectionHtml),    
    Html = div(class='page-layout', [
        div(class='book-section',[
            div(class='padding-left',''),
            div(class='section-body', SectionHtml),
            div(class='padding-right','')]), 
        div([id(modal), class(modal)], [
            span(class('modal-close'), x ),
            img([class('modal-content'), id('modal-img')]),
            div(id('modal-caption'), '')]), 
        script([src='/_themes/modal.js'], '')]).

%public
Manifest.render_head(BaseUrl) :=  Html :-
    atomic_list_concat(['/_themes/', Manifest.theme, '.css'], Theme0),
    rebase_absolute_path(BaseUrl, Theme0, ThemeLink),
    atomic_list_concat(['/_themes/', 'base.css'], ThemeBase),
    rebase_absolute_path(BaseUrl, ThemeBase, ThemeBaseLink),        
    Html = 
        [ 
            link([ type('text/css'), rel(stylesheet), href('https://cdnjs.cloudflare.com/ajax/libs/perfundo/4.0.4/perfundo.with-icons.min.css') ]),
            link([ type('text/css'), rel(stylesheet), href('//cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/styles/default.min.css') ]),
            link([ type('text/css'), rel('stylesheet'), href(ThemeBaseLink) ]),
            link([ type('text/css'), rel('stylesheet'), href(ThemeLink) ]),                                
            script([src='//cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/highlight.min.js'], ''),                
            script('hljs.initHighlightingOnLoad();'),
            title(Manifest.title)
        ].

% public
Manifest.resolve_section_path(SectionIn) := Section :-
    directory_file_path(Manifest.base, Manifest.'source-root', BookBase),
    directory_file_path(BookBase, SectionIn, SectionPath),
    access_file(SectionPath, read),
    (
        exists_directory(SectionPath),
        !,
        directory_file_path(SectionIn, Manifest.index, Section1),
        Section = Manifest.resolve_section_path( Section1 )
    ;
        Section = SectionPath
    ).  

% public
load_documentation_manifest(Request, ManifestOut) :-
    default_manifest(Manifest),
    Manifest1 = Manifest.put(_{ base: documentation, title: 'render-md documentation'}),
    http_parameters(Request, [ theme(Theme, [default(Manifest1.theme)])] ),
    ManifestOut = Manifest1.put(theme, Theme). 

% public 
load_manifest(Request, ManifestOut) :-
    default_manifest(Manifest),
    directory_file_path(Manifest.base, 'book.json', ManifestFile),
    (
        % try to load user's manifest
        access_file(ManifestFile, read),
        open(ManifestFile, read, Stream, [encoding(utf8)]),
        json_read_dict(Stream, BookDict),
        close(Stream),
        Manifest1 = Manifest.put(BookDict)

    ;   % if book.json does not exists, check if the user folder is empty     
        ( 
            \+ exists_directory(Manifest.base) 
        ;
            is_user_folder_empty(Manifest)
        ),
        % and fallback to built-in documentation
        Manifest1 = Manifest.put(_{ base: documentation, title: 'render-md documentation'})
    ;   % handle with default book.json (the manifest is not mandatory by the specification)
        Manifest1 = Manifest
    ),      
    http_parameters(Request, [ theme(Theme, [default(Manifest1.theme)])] ),
    ManifestOut = Manifest1.put(theme, Theme).

% private 
default_manifest(DefaultDict) :- 
    setting(book_base, BookRoot),

    DefaultDict = md_manifest{
        'source-root': '.',
        title: 'Book',        
        base: BookRoot, 
        index: 'README.md',
        summary: 'SUMMARY.md',
        links: 'LINKS.md',
        theme: default
        
    }.

% private
is_user_folder_empty(Manifest) :-
    Dir = Manifest.base, 
    directory_files(Dir, Files),
    (delete(Files, '.', Files1) ; Files1 = Files),
    (delete(Files1, '..', Files2) ; Files2 = Files1),
    length(Files2, 0).

rebase_absolute_path(BaseUrl, Path, Rebased) :-
    (
        atom_concat(BaseUrl1,'/', BaseUrl)
    ;
        BaseUrl1 = BaseUrl
    ),
    atom_concat(BaseUrl1, Path, Rebased),
    !. 



