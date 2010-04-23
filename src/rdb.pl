:- module(rdb, [connect/1,table/1,table_column/2,query/2,uri_to_id/2, sanitise/2, add_table_to_cache/1, add_table_column_to_cache/2]).

:- use_module(library(odbc)).

:- dynamic cached_table/1.
:- dynamic cached_table_column/2.

connect(DSN) :-
        empty_cache,
        odbc_connect(DSN, _, [
            user(root),
            alias(store),
            open(once)
        ]),
        cache_schema.

table(T) :-
    cached_table(T).

table_column(T, C) :-
    cached_table_column(T, C).

empty_cache :-
    retractall(cached_table(_)),
    retractall(cached_table_column(_, _)).
cache_schema :-
    forall(odbc_current_table(store, T), add_table_to_cache(T)),
    forall(odbc_table_column(store, T, C), add_table_column_to_cache(T, C)).

add_table_to_cache(T) :-
    assert(cached_table(T)).
add_table_column_to_cache(T, C) :-
    assert(cached_table_column(T, C)).

query(SQL, Results) :-
    odbc_query(store, SQL, Results).

uri_to_id(URI, ID) :-
    sanitise(URI, URIS),
    sformat(SQL, 'SELECT id FROM `owl:Thing` WHERE uri = \'~w\'', [URIS]),
    query(SQL, row(ID)), !.
uri_to_id(URI, ID) :-
    sanitise(URI, URIS),
    sformat(SQL, 'REPLACE INTO `owl:Thing` (`uri`) VALUES (\'~w\')', [URIS]),
    query(SQL, _),
    uri_to_id(URI, ID).

sanitise(L, LL) :-
    replace(L, '\'', '\\\'', L1),
    replace(L1, '’', '\\\'', LL).
replace(S, C, R, Result) :-
    atomic_list_concat(List, C, S),
    atomic_list_concat(List, R, Result).
