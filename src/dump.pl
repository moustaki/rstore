:- module(dump, [dump_rdfxml/0]).

:- use_module(rdb_entailment).
:- use_module(library(rdf_write)).

dump_rdfxml :-
    findall(rdf(S, P, O), rdb_entailment:rdf(S, P, O), Triples),
    rdf_write_xml(current_output, Triples).
