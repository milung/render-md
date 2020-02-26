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
    body_html(BaseUrl, Manifest.title, SummaryHtml, SectionHtml, Html).

Manifest.render_body(BaseUrl, Section) := Html :-
    md_book(Manifest, Section, SectionHtml),    
    body_html(BaseUrl, Manifest.title, [], SectionHtml, Html).

body_html(BaseUrl, Title,  SummaryHtml, SectionHtml, Html) :-    
    atomic_list_concat(['/_themes/', 'app.js'], AppBase),
    rebase_absolute_path(BaseUrl, AppBase, AppBaseLink), 
    toolbar_html(Title, Toolbar),
    drawer_html(SummaryHtml, Drawer),
    modal_html(Modal),
    Html = 
        body( class='mdc-typography', 
        [    
            Toolbar,
            Drawer,
            div(class='mdc-drawer-app-content', [
                        
                div(class='mdc-top-app-bar--dense-fixed-adjust book-content',SectionHtml)
            ]),                    
            Modal, 
            script([src='//cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/highlight.min.js'], ''),                
            script([src='https://unpkg.com/material-components-web@latest/dist/material-components-web.min.js'], ''),
            script([src=AppBaseLink], '')
        ]).

toolbar_html(Title, Html) :-
    Html = header( [ class='mdc-top-app-bar mdc-top-app-bar--dense', style='top: 0px;'], 
            div( class= 'mdc-top-app-bar__row', [
                section(class='mdc-top-app-bar__section mdc-top-app-bar__section--align-start', [
                    button([                         
                        class='mdc-icon-button material-icons mdc-top-app-bar__navigation-icon mdc-ripple-upgraded--unbounded mdc-ripple-upgraded', 
                        style='--mdc-ripple-fg-size:28px; --mdc-ripple-fg-scale:1.71429; --mdc-ripple-left:10px; --mdc-ripple-top:10px;',
                        onclick='onDrawerToggle()'], 
                        [menu]),
                    span( class='mdc-top-app-bar__title', Title)
                ]),
                section(class='mdc-top-app-bar__section mdc-top-app-bar__section--align-end', [
                    button([ 
                        class='mdc-icon-button material-icons mdc-top-app-bar__action-item mdc-ripple-upgraded--unbounded mdc-ripple-upgraded', 
                        aria-label='Download',
                        style='--mdc-ripple-fg-size:28px; --mdc-ripple-fg-scale:1.71429; --mdc-ripple-left:10px; --mdc-ripple-top:10px;'],
                        [file_download]),
                    button([ 
                        class='mdc-icon-button material-icons mdc-top-app-bar__action-item mdc-ripple-upgraded--unbounded mdc-ripple-upgraded', 
                        aria-label='Print this page',
                        style='--mdc-ripple-fg-size:28px; --mdc-ripple-fg-scale:1.71429; --mdc-ripple-left:10px; --mdc-ripple-top:10px;'],
                        [print])
                ])
            ])).

drawer_html(SummaryHtml, Html) :-
    Html = 
        aside( [id=drawer, class='mdc-drawer mdc-drawer--dismissible mdc-drawer--open'], [
            div(class='mdc-drawer__header', [
                h3(class='mdc-drawer__title', 'Table of contents')
            ]),                
            div(class='mdc-drawer__content', nav(class='mdc-list', SummaryHtml))
        ]).

modal_html(
div( [ class=modal, id=modal],
        div(class='mdc-card', [
            
            div([class='mdc-card__primary-action', tabindex=0], [
                h2([ id='modal-caption', class='mdc-typography mdc-typography--headline6'], ''),
                div([ class='mdc-card__media mdc-card__media--16-9  modal-media',
                      id='modal-image'],'')
            ]),
            div(class='mdc-card__action-icons', [
                div(class='mdc-card__action-buttons',
                    button(class='mdc-button mdc-card__action mdc-card__action--button modal-close','Close'))
            ])
        ]))
).
    
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
            link([type('text/css'), rel(stylesheet), href('https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css')]),
            link([rel(stylesheet), href('https://fonts.googleapis.com/icon?family=Material+Icons')]),
            link([rel(stylesheet), href('https://fonts.googleapis.com/css?family=Rubik:300,400,500')]),
            link([rel(stylesheet), href('https://fonts.googleapis.com/css?family=Roboto+Mono')]),
            link([rel(stylesheet), href('https://fonts.googleapis.com/css?family=Roboto:300,400,500,600,700')]),            
            link([ type('text/css'), rel('stylesheet'), href(ThemeBaseLink) ]),
            link([ type('text/css'), rel('stylesheet'), href(ThemeLink) ]),                                
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



