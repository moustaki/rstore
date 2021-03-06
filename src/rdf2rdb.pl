:- module(rdf2rdb, [load_rdf_in_db/2, reduce/2, expand/2]).

:- use_module(library('rdf')).
:- use_module(library('odbc')).
:- use_module(rdb).

:- style_check(-atom).

load_rdf_in_db(File, URI) :-
    init_db,
    load_rdf(File, Triples, [namespaces(N), base_uri(URI)]),
    handle_namespaces(N,Namespaces),
    handle_triples(Triples, Namespaces).


handle_triples([], _) :- !.
handle_triples([rdf(S,P,O)|T], Ns) :-
    reduce(S,SS),
    reduce(P,SP),
    reduce(O,SO),
    forall(
        triple_schema_change(rdf(SS, SP, SO), Change), (
            write(Change),
            write('\n'),
            query(Change, _)
        )
    ),
    forall(
        triple_insert(rdf(SS, SP, SO), Insert), (
            write(Insert),
            write('\n'),
            query(Insert, _)
        )
    ),
    handle_triples(T, Ns).

reduce(URI, Short) :-
    atomic(URI),
    namespace(S, L),
    atom_concat(L, Suffix, URI),
    atomic_list_concat([S, ':', Suffix], Short), !.
reduce(Var1, Var2) :-
    var(Var1), var(Var2), !.
reduce(Term, Term).

expand(Short, URI) :-
    atomic(Short),
    namespace(S, L),
    atomic_list_concat([S, Suffix], ':', Short),
    atom_concat(L, Suffix, URI), !.
expand(Var1, Var2) :-
    var(Var1), var(Var2), !.
expand(Term, Term).

handle_namespaces([],[]) :- !.
handle_namespaces([Short=Long|Tail], [Short=Long|Rest]) :-
    namespace(Short, Long),
    handle_namespaces(Tail, Rest).
handle_namespaces([Short=Long|Tail], [Short=Long|Rest]) :-
    \+ namespace(Short, _),
    \+ namespace(_, Long),
    sformat(SQL, 'INSERT INTO `namespaces` (`short`, `long`) VALUES (\'~w\', \'~w\')', [Short, Long]),
    query(SQL, _),
    handle_namespaces(Tail, Rest).
handle_namespaces([Short=Long|Tail], [NShort=Long|Rest]) :-
    namespace(NShort, Long),
    \+ (NShort = Short),
    handle_namespaces(Tail, Rest).
handle_namespaces([Short=Long|Tail], [NShort=Long|Rest]) :-
    namespace(Short, OtherLong),
    \+ (OtherLong = Long),
    atom_concat(Short, '_', NShort),
    sformat(SQL, 'INSERT INTO `namespaces` (`short`, `long`) VALUES (\'~w\', \'~w\')', [NShort, Long]),
    query(SQL, _),
    handle_namespaces(Tail, Rest).
    

init_db :-
    query('CREATE TABLE IF NOT EXISTS `namespaces` (
        `id`     int(11) NOT NULL auto_increment,
        `short`  varchar(255)    NOT NULL,
        `long`   varchar(255)    NOT NULL,
        PRIMARY KEY (`id`),
        UNIQUE KEY `index_namespaces_on_short` (`short`),
        UNIQUE KEY `index_namespaces_on_long`  (`long`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', _),
    query('REPLACE INTO `namespaces` (`short`, `long`) VALUES (\'owl\', \'http://www.w3.org/2002/07/owl#\')', _),
    query('CREATE TABLE IF NOT EXISTS `owl:Thing` (
        `id`   int(11)      NOT NULL auto_increment,
        `uri`  varchar(255) NOT NULL,
        PRIMARY KEY (`id`),
        UNIQUE KEY `index_owl:Thing_on_uri` (`uri`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', _).

namespace(Short, Long) :-
    query('SELECT `short`, `long` FROM `namespaces`', row(Short, Long)).


% handling schema changes when first encountering types and properties
% adding a type
triple_schema_change(rdf(_, 'rdf:type', Object), SQL) :-
    !,
    \+ table(Object),
    sformat(SQL, 'CREATE TABLE `~w` (
        `id`  int(11)      NOT NULL auto_increment,
        `uri` varchar(255) NOT NULL,
        PRIMARY KEY (`id`),
        UNIQUE KEY `index_~w_on_uri` (`uri`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', [Object,Object]), 
    % WARNING - writing to the cache just before the change is actually applied
    add_table_to_cache(Object), !.
% adding a property
triple_schema_change(rdf(_, Property, _), SQL) :-
    \+ table(Property),
    sformat(SQL, 'CREATE TABLE `~w` (
        `id`             int(11) NOT NULL auto_increment,
        `subject_id`     int(11) NOT NULL,
        `value`          text    DEFAULT NULL,
        `xsd_type`       varchar(255) DEFAULT NULL,
        `lang`           varchar(255) DEFAULT NULL,
        `object_id`      int(11) DEFAULT NULL,
        PRIMARY KEY (`id`),
        UNIQUE KEY (`subject_id`, `value`(10)),
        UNIQUE KEY (`subject_id`, `object_id`),
        KEY `index_~w_on_subject_id` (`subject_id`),
        KEY `index_~w_on_object_id` (`object_id`),
        KEY `index_~w_on_lang` (`lang`),
        KEY `index_~w_on_value` (`value`(10)),
        INDEX `index_~w_on_subject_id_and_object_id` (`subject_id`, `object_id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', [Property,Property,Property,Property,Property,Property,Property]), 
    % WARNING - writing to the cache just before the change is actually applied
    add_table_to_cache(Property), !.
% ...and that's all we need for now

% handling triple insert
% type insert
triple_insert(rdf(S, 'rdf:type', O), SQL) :-
    !,
    table(O),
    (sformat(SQL, 'REPLACE INTO `~w` (`uri`) VALUES (\'~w\')', [O, S, S]);
     sformat(SQL, 'REPLACE INTO `owl:Thing` (`uri`) VALUES (\'~w\')', [S])).
% untyped literal insert
triple_insert(rdf(S, P, literal(L)), SQL) :-
    \+(L = type(_,_)),
    \+(L = lang(_,_)),
    !,
    table(P),
    uri_to_id(S, ID),
    sanitise(L, LL),
    sformat(SQL, 'REPLACE INTO `~w` (`subject_id`, `value`) VALUES (~w, \'~w\')', [P, ID, LL]).
% generic typed literal insert
triple_insert(rdf(S, P, literal(type(T, V))), SQL) :-
    !,
    table(P),
    uri_to_id(S, ID),
    sanitise(V, VV),
    sformat(SQL, 'REPLACE INTO `~w` (`subject_id`, `value`, `xsd_type`) VALUES (~w, \'~w\', \'~w\')', [P, ID, VV, T]).
% generic lang literal insert
triple_insert(rdf(S, P, literal(lang(L, V))), SQL) :-
    !,
    table(P),
    uri_to_id(S, ID),
    sanitise(V, VV),
    sformat(SQL, 'REPLACE INTO `~w` (`subject_id`, `value`, `lang`) VALUES (~w, \'~w\', \'~w\')', [P, ID, VV, L]).
% object property
triple_insert(rdf(S,P,O), SQL) :-
    !,
    table(P),
    uri_to_id(S, SID),
    uri_to_id(O, OID),
    sformat(SQL, 'REPLACE INTO `~w` (`subject_id`, `object_id`) VALUES (~w, ~w)', [P, SID, OID]).

