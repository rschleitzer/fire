; appmigrations.scm - hook for application migrations in the database provider.
;
; A project may apply every migration-*.json of its seed once and record it in dbinfo; its own
; appmigrations.scm then defines the two SQL members for that. A server generated from this
; shared generator without that feature includes this file instead and gets no members, so
; dbprovider.scm can call the hook unconditionally.

; Interface lines of IDatabaseProvider, inserted after InsertVersionSql.
(define (dbp-application-migration-interface) "")

; Implementation members of DatabaseProvider for one database, inserted after InsertVersionSql.
(define (dbp-application-migration-sql db) "")
