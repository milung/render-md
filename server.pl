:- module(server, [server/0, server/1, server_start_and_wait/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/json)).
:- use_module(library(http/http_header)).

:- use_module('md/md_manifest').
:- use_module('md/md_parse').
:- use_module('md/md_book').

:- multifile 
    user:file_search_path/2,
    http:status_page/3,
    http:location/3.

:- dynamic   
    http:location/3.


:- setting(port, number, env(http_port, 80), 'HTTP port the server is listening on. (ENV http_port)').
:- setting(server_base_url, atom, env(base_url, '/'), 'Base URL of the server, all absolute links are prefixed with this address (ENV base_url)').

user:file_search_path(book, BookBase) :- setting(md_manifest:book_base, BookBase).
user:file_search_path(book, './documentation').

user:file_search_path(themes, './themes').

:- http_handler(root('_themes'), serve_theme, [prefix]).
:- http_handler(root('_help'), serve_help, [prefix]).

:- http_handler(root('favicon.ico'), http_reply_file('themes/favicon.ico', []), []).
:- http_handler(root(.), serve_book, [prefix]).

server :-     
    setting(port, Port),
    server(Port).

server(Port) :- 
    http_server(http_dispatch, [port(Port)]).

server_start_and_wait :-
    setting(port, Port),
    server(Port),
    repeat,
    sleep(3),
    \+ is_server_dead(Port).

is_server_dead(Port) :- 
    http_current_worker(Port, _ ), 
    ! .   

serve_help(Request) :-
    load_documentation_manifest(Request, Manifest),
    serve_book(Manifest, Request).

serve_book(Request) :-
    load_manifest(Request, Manifest),
    serve_book(Manifest, Request).

serve_book(Manifest, Request) :-
    option(path_info(SectionUri), Request, Manifest.index),
    uri_encoded(path, Section, SectionUri),
    
    SectionPath = Manifest.resolve_section_path(Section),    
    file_mime_type(SectionPath, MimeType),
    (
        MimeType = text/markdown,        
        setting(server_base_url,BaseUrl),
        reply_html_page(
            material,
            Manifest.render_head(BaseUrl), 
            Manifest.render_body(BaseUrl, SectionPath))           
    ;          
        AssetPath = Manifest.asset_path(Section),
        http_reply_file(AssetPath, [], Request)
    ).

serve_book(_Manifest, Request) :-
    option(path(Path), Request),
    throw(http_reply(not_found(Path))).


serve_theme( Request) :-
    option(path_info(Asset), Request),
    absolute_file_name(themes(Asset), Absolute),
    exists_file(Absolute),    
    access_file(Absolute, read),
    http_reply_file(themes(Asset), [], Request).
serve_theme(Request) :-
    option(path(Path), Request),
    throw(http_reply(not_found(Path))).

http:status_page(not_found(URL), _Context, HTML) :-
    phrase(page([ title('Sorry, no such page')], 
    {|html(URL) || <h1>Sorry, no such page <span>URL</span></h1>|} ), HTML).
