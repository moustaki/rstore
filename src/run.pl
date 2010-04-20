#!/usr/local/bin/pl -G32M -T32M -L32M -s

:- use_module('rdb').
:- use_module('rdb_entailment').
:- assert(library_directory('./SeRQL/lib')).

:- ['SeRQL/server.pl'].

:- user_db:set_user_database('users.db').
:- serql_server(1234, []).

% When in the Prolog prompt, run connect(DSN).
