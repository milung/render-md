%%% Loads all source modules to be used on top level
:-  prolog_load_context(directory, Dir), 
    asserta(user:file_search_path(project, Dir)),
    directory_file_path(Dir, sources, SourcePath),
    asserta(user:file_search_path(source, SourcePath)),
    directory_file_path(Dir, assets, AssetsPath),
    asserta(user:file_search_path(asset, AssetsPath)).

:- set_prolog_flag(encoding, utf8).

:- use_module(library(settings)).

:- use_module(library(debug)).
% debugging is enabled for info, warnig, and error levels 
% - use debug and trace for more glanular levels
:- debug(info).
:- debug(info(_)).
:- debug(warning).
:- debug(error).
:- debug(warning(_)).
:- debug(error(_)).

:- use_module(server).
:- use_module(source(commonmark/line)).
:- use_module(source(commonmark/wildcards)).
:- use_module(source(commonmark/blocks)).
:- use_module(source(commonmark/leaf_blocks)).
:- use_module(source(commonmark/code_blocks)).
:- use_module(source(html_blocks)).
%source(routing)).
%source(main)).
