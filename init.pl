%% used to initialize the prolog environment

:-  prolog_load_context(directory, Dir), 
    asserta(user:file_search_path(project, Dir)),
    directory_file_path(Dir, sources, SourcePath),
    asserta(user:file_search_path(source, SourcePath)),
    directory_file_path(Dir, assets, AssetsPath),
    asserta(user:file_search_path(asset, AssetsPath)).

:- set_prolog_flag(encoding, utf8).