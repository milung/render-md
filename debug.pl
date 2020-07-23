:- [load].

% debug settings to make debugging more fancy
:- use_module(library('http/http_error')).

:- load_settings('app.settings.pl', [undefined(load)]).
:- load_settings('dev.settings.pl', [undefined(load)]).

:- load_test_files([]).

% generic debugging
:- debug(trace).
:- debug(trace(_)).
:- debug(debug).
:- debug(debug(_)).
:- debug(info).
:- debug(info(_)).
:- debug(warning).
:- debug(warning(_)).
:- debug(error).
:- debug(error(_)).
