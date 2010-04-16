:- module(rdf2rdb).

:- use_module(library('rdf')).
:- use_module(library('odbc')).
:- use_module(rdb).

:- style_check(-atom).

triple_to_sql(Triple, SQL) :-
    triple_schema_change(Triple, SQL);
    triple_insert(Triple, SQL).

% handling schema changes when first encountering types and properties
% adding a type
triple_schema_change(rdf(_, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Object), SQL) :-
    \+table(Object),
    sformat(SQL, 'CREATE TABLE `~w` (
        `id`  int(11) NOT NULL auto_increment,
        `uri` int(11) NOT NULL,
        PRIMARY KEY (`id`),
        UNIQUE KEY `index_~w_on_uri` (`uri`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', [Object,Object]), !.
% adding an untyped literal property
triple_schema_change(rdf(_, Property, literal(L)), SQL) :-
    \+table(Property),
    \+(L = type(_,_)),
    sformat(SQL, 'CREATE TABLE `~w` (
        `id`         int(11) NOT NULL auto_increment,
        `subject_id` int(11) NOT NULL,
        `value`      text    NOT NULL,
        PRIMARY KEY (`id`),
        KEY `index_~w_on_subject_id` (`subject_id`),
        KEY `index_~w_on_value` (`value`),
        INDEX `index_~w_on_subject_id_and_value` (`subject_id`, `value`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', [Property,Property,Property,Property]), !.
% adding an object property
triple_schema_change(rdf(_, Property, _), SQL) :-
    \+table(Property),
    sformat(SQL, 'CREATE TABLE `~w` (
        `id`         int(11) NOT NULL auto_increment,
        `subject_id` int(11) NOT NULL,
        `object_id`  int(11) NOT NULL,
        PRIMARY KEY (`id`),
        KEY `index_~w_on_subject_id` (`subject_id`),
        KEY `index_~w_on_object_id` (`object_id`),
        INDEX `index_~w_on_subject_id_and_object_id` (`subject_id`, `object_id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8', [Property,Property,Property,Property]), !.
% ...and that's all we need for now

% handling triple insert
% type insert
triple_insert(rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', O), SQL) :-
    table(O),
    sformat(SQL, 'INSERT INTO `~w` (`uri`) VALUES (\'~w\')', [S]).
% untyped literal insert
triple_insert(rdf(S, P, literal(L)), SQL) :-
    table(P),
    \+(L = type(_,_)),
    uri_to_id(S, ID),
    sformat(SQL, 'INSERT INTO `~w` (`subject_id`, `value`) VALUES (\'~w\', \'~w\')', [P, ID, L]).
% object property
triple_insert(rdf(S,P,O), SQL) :-
    table(P),
    uri_to_id(S, SID),
    uri_to_id(O, OID),
    sformat(SQL, 'INSERT INTO `~w` (`subject_id`, `object_id`) VALUES (\'~w\', \'~w\')', [P, SID, OID]).
