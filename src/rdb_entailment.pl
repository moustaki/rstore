:- module(rdb_entailment, []).

:- use_module(rdf2rdb).
:- use_module(rdb).
:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
        rdf_bnode/1,
        rdf_is_bnode/1,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module('SeRQL/sparql_runtime.pl').

rdf(S, P, O) :-
    reduce(S, SS),
    reduce(P, SP),
    reduce(O, SO),
    rdb(SS, SP, SO),
    expand(SS, S),
    expand(SP, P),
    expand(SO, O).

rdb(S, P, literal(type(T, V))) :-
    \+ (P == 'rdf:type'),
    table_column(P, C),
    C = 'value',
    sformat(SQL, 'SELECT t.uri, p.value, p.xsd_type FROM `~w` p INNER JOIN `owl:Thing` t ON t.id = p.subject_id WHERE p.xsd_type IS NOT NULL', [P]),
    write(SQL), write('\n'),
    query(SQL, row(S, V, T)).
rdb(S, P, literal(lang(L, V))) :-
    \+ (P == 'rdf:type'),
    table_column(P, C),
    C = 'value',
    sformat(SQL, 'SELECT t.uri, p.value, p.lang FROM `~w` p INNER JOIN `owl:Thing` t ON t.id = p.subject_id WHERE p.lang IS NOT NULL', [P]),
    write(SQL), write('\n'),
    query(SQL, row(S, V, L)).
rdb(S, P, literal(L)) :-
    \+ (P == 'rdf:type'),
    \+ (L == lang(_, _)),
    \+ (L == type(_, _)),
    table_column(P, C),
    C = 'value',
    sformat(SQL, 'SELECT t.uri, p.value  FROM `~w` p INNER JOIN `owl:Thing` t ON t.id = p.subject_id WHERE p.lang IS NULL AND p.xsd_type IS NULL', [P]),
    write(SQL), write('\n'),
    query(SQL, row(S, L)).
rdb(S, P, O) :-
    \+ (P == 'rdf:type'),
    table_column(P, C),
    C = 'object_id',
    sformat(SQL, 'SELECT s.uri, o.uri FROM `~w` p INNER JOIN `owl:Thing` s ON s.id = p.subject_id INNER JOIN `owl:Thing` o ON o.id = p.object_id', [P]),
    write(SQL), write('\n'),
    query(SQL, row(S, O)).
rdb(S, 'rdf:type', O) :-
    atomic(S), atomic(O), !,
    table(O),
    sformat(SQL, 'SELECT `uri` FROM `~w` WHERE `uri` = \'~w\'', [O, S]),
    write(SQL), write('\n'),
    query(SQL, row(S)).
rdb(S, 'rdf:type', O) :-
    var(S), !,
    table_column(O, C), % not sure why backtracking here fails
    C = 'uri',
    sformat(SQL, 'SELECT `uri` FROM `~w`', [O]),
    write(SQL), write('\n'),
    query(SQL, row(S)).


/**
 * We'll know register the rdf/3 predicate to 
 * be used as an entailment module within the
 * SeRQL SWI Semantic Web server.
 */


                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
        serql:entailment/2.

serql:entailment(rdb, rdb_entailment).
