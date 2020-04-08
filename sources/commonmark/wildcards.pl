:- module(wildcards, [
    at_interval//3,         % +Min:integer, +Max:integer, +Element
    at_interval_count//4,   % +Min:integer, +Max:integer, +Element, -Actual_integer
    at_least//2,            % +Occurences:integer, +Element
    at_least_count//3,      % +Occurences:integer, +Element, -Actual_integer
    at_most//2,             % +Occurences:integer, +Element
    at_most_count//3,       % +Max:integer, +Element:term, -Actual:integer
    count_or_less//2,       % +Count:integer, +Element:term
    one_or_max//2,          % +Max:integer, +Element:term
    one_or_max_count//3,    % +Max:integer, +Element:term, -Actual:integer
    one_or_more//1,         % +Element:term
    space//0,
    zero_or_max//2,         % +Max:integer, +Element:term
    zero_or_max_count//3,   % +Max:integer, +Element:term, -Actual:integer
    zero_or_more//1         % +Element:term
]).
%! <module> wildcards doing some cool things
%  Predicates for wildcards ...

:- meta_predicate 
    at_interval(+, +, //, ?,?),
    at_interval_count(+, +, //, -, ?,?),
    at_least(+, //, ?,?),
    at_least_count(+, //, -, ?,?),
    at_most(+, //, ?,?),
    at_most_count(+, //, -, ?,?),
    count_or_less(+, //, ?, ?),
    one_or_max(+, //, ?,?),     
    one_or_max_count(+, //,-, ?,?),
    one_or_more( //, ?,?),    
    zero_or_max(+, //, ?,?),
    zero_or_max_count(+, //, -, ?, ?),
    zero_or_more(//, ?,?).    

:- encoding(utf8).

:- use_module(library(dcg/basics)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! at_interval(+Min:integer, +Max:integer, +Element:term)// is nondet
%  Eagerly matches not least than Min and not more that Max occurences of the Element in the input
%  Fails if there are less or more than occurences
at_interval(Min, Max, X) -->
    at_interval_count(Min, Max, X, _).

%! at_interval_count(+Min:integer, +Max:integer, +Element:term, -Actual:integer)// is nondet
%  Eagerly matches not least than Min and not more that Max occurences of the Element in the input
%  Fails if there are less or more than occurences
%  Unifies Actual with the actual count of occurences.
at_interval_count(Min, Max, X, Actual) -->
    at_most_count_impl(0, Max, X, Actual),
    {
        Actual >= Min 
    }.

%! at_least(+Occurences:integer, +Element)// is nondet
%  Lazily matches at least specified amount of Occurences or more. 
at_least(Count, X) --> 
    at_least_count(Count, X, _).

%! at_least_count(+Occurences:integer, +Element, -Actual_integer)// is nondet
%  Lazily matches at least specified amount of Occurences or more.
%  Unifies  Actual with the actual number of matched occurences.   
at_least_count(Min, X, Actual) -->
    at_least_count_impl(0, Min, X, Actual).

%! at_most(+Max:integer, +Element:term)// is nondet
%  Eagerly matches at maximum Max occurences of the Element in the input
%  Fails if there are more than Max occurences
at_most(Max, X) -->
    at_most_count(Max, X, _).

%! at_most_count(+Max:integer, +Element:term, -Actual:integer)// is nondet
%  Eagerly matches at maximum Max occurences of the Element in the input, and 
%  unifies Actual with the actual count of occurences.
%  Fails if there are more than Max occurences
at_most_count(Max, X, Actual) -->
    at_most_count_impl(0, Max, X, Actual).

%! count_or_less(+Count:integer, +Element:term)// is det
%  Eagerly matches Count or less occurences of the Element in the input, 
% leaving consequent occurences in the stream without failing if there are 
% more than Count occurences
count_or_less(Count, X) --> 
    { 
        Count > 0,
        Count1 is Count -1
    },
    X, 
    count_or_less(Count1, X).
count_or_less(_, _) --> [], !.

%! one_or_max(+Max:integer, +Element:term)// is nondet
%  Eagerly matches one or maximum Max occurences of the Element in the input. 
%  Fails if there are more than Max occurences
one_or_max(Count, X) -->
    one_or_max_count( Count, X, _).

%! one_or_max_count(+Max:integer, +Element:term, -Actual:integer)// is nondet
%  Eagerly matches one or maximum Max occurences of the Element in the input, and 
%  unifies Actual with the actual count of occurences.
%  Fails if there are more than Max occurences
one_or_max_count(Max, X, Actual) -->
    at_interval_count(1, Max, X, Actual).

%! one_or_more(+Element:term)// is nondet
%  Lazily matches at least one or any other amount of Element occurences in the input
one_or_more(X) --> at_least(1, X).

%! space// is det
%  simple space definition
space --> [ 32 ].

%! zero_or_max(+Max:integer, +Element:term)// is nondet
%  Lazily matches zero or any or maximu Max of Element occurences in the input
zero_or_max(Max, X) --> zero_or_max_count(Max, X, _).

%! zero_or_max_count(+Max:integer, +Element:term, -Actual:integer)// is nondet
%  Eagerly matches zero or maximum Max occurences of the Element in the input, and 
%  unifies Actual with the actual count of occurences.
%  Fails if there are more than Max occurences
zero_or_max_count(_, X, 0) --> \+ X.
zero_or_max_count(Max, X, Actual) -->
    at_interval_count(0, Max, X, Actual).

%! zero_or_more(+Element:term)// is nondet
%  Lazily matches zero or maximum Max occurences of the Element in the input. 
%  Fails if there are more than Max occurences
zero_or_more(X) --> at_least(0, X).

%! zero_or_more_count(+Element:term, -Actual:integer)// is nondet
%  Lazily matches zero or maximum Max occurences of the Element in the input. 
%  Fails if there are more than Max occurences. Unifies Actual with the
%  actual count of occurences
zero_or_more(X, Count) --> at_least_count(0, X, Count).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%
at_most_count_impl(Counter, Max, X, Counter) -->
    { Counter =< Max }, 
    \+ X.

at_most_count_impl(Counter, Max, X, Actual) -->
    {
        Counter < Max,
        Counter1 is Counter + 1
    },
    X,
    at_most_count_impl(Counter1, Max, X, Actual).

at_least_count_impl(Counter, Min, _, Counter) --> 
    {
        Counter >= Min
    },
    [].
    
at_least_count_impl(Counter, Min, X, Actual) -->
    X, 
    {
        Counter1 is Counter + 1
    },
    at_least_count_impl(Counter1, Min, X, Actual).
