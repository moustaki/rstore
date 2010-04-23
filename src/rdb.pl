:- module(rdb, [connect/1,table/1,table_column/2,query/2,uri_to_id/2, sanitise/2]).

:- use_module(library(odbc)).

connect(DSN) :-
        odbc_connect(DSN, _, [
        user(root),
        alias(store),
        open(once)
    ]).

table(T) :-
    odbc_current_table(store, T).

table_column(T, C) :-
    odbc_table_column(store, T, C).

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
