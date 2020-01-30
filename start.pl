:- [load].

:- use_module(server, [server/0]).

:-  writeln('render.MD v0.1.0, January 2020'),
    writeln(''),
    list_settings(server), 
    server_start_and_wait.