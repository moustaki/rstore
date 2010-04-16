:- module(rdb, [connect/1,table/1,table_column/2,query/2,uri_to_id/2]).

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
    table_column(T, Column), % for some reason, forcing Column = 'uri' does not backtrack correctly
    Column = 'uri', 
    sformat(SQL, 'SELECT id FROM `~w` WHERE uri = \'~w\'', [T, URI]),
    query(SQL, row(ID)).

