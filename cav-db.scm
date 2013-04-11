;;; cav-db.scm -- A sqlite3 database layer for Coq au Vin.
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module cav-db
        *
        (import scheme chicken)
        (import extras)

        (use utf8)
        (use utf8-srfi-13)
        (use sql-de-lite)
        (use sets)
        (use (prefix dbstrings dbs:))


(define current-db (make-parameter #f))

(define first-id (make-parameter (lambda (_) 0)))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  SQL QUERIES  -----------------------------------------------------

;;; ------  Initial Setup  -------------------------------------------------

(define setup-queries
  '(
#<<SQL
CREATE TABLE statements (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s TEXT NOT NULL,
    p TEXT NOT NULL,
    o TEXT NOT NULL
);
SQL

#<<SQL
CREATE TABLE lists (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    list_id TEXT NOT NULL,
    item TEXT NOT NULL
);
SQL

#<<SQL
CREATE TABLE sets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    set_id TEXT NOT NULL,
    item TEXT NOT NULL
);
SQL

#<<SQL
CREATE TABLE hashes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    hash_id TEXT NOT NULL,
    key TEXT NOT NULL,
    value TEXT NOT NULL
);
SQL

#<<SQL
CREATE TABLE vocabularies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    vocab_name TEXT NOT NULL,
    vocab_desc TEXT,
    term TEXT NOT NULL
);
SQL


"CREATE INDEX idx_s ON statements(s);"

"CREATE INDEX idx_p ON statements(p);"

"CREATE INDEX idx_o ON statements(o);"

"CREATE INDEX idx_sp ON statements(s,p);"

"CREATE INDEX idx_so ON statements(s,o);"

"CREATE INDEX idx_op ON statements(o,p);"

"CREATE INDEX idx_spo ON statements(s,p,o);"

"CREATE INDEX idx_lists ON lists(list_id);"

"CREATE INDEX idx_sets ON sets(set_id);"

"CREATE INDEX idx_hashes ON hashes(hash_id);"

"CREATE INDEX idx_vocabs ON vocabularies(vocab_name);"

))

(define s->p+o-query "SELECT p, o FROM statements WHERE s = '?';")

(define p->s+o-query "SELECT s, o FROM statements WHERE p = '?';")

(define o->s+p-query "SELECT s, p FROM statements WHERE o = '?';")

(define s+p->o-query "SELECT o FROM statements WHERE s = '?' AND p = '?';")

(define p+o->s-query "SELECT s FROM statements WHERE p = '?' AND o = '?';")

(define s+o->p-query "SELECT p FROM statements WHERE s = '?' AND o = '?';")

(define get-list-query "SELECT item FROM lists WHERE list_id = '?' ORDER BY id;")

(define get-set-query "SELECT item FROM sets WHERE set_id = '?';")

(define get-hash-query "SELECT key, value FROM hashes WHERE hash_id = '?';")

(define get-vocab-query "SELECT term FROM vocabularies WHERE vocab_name = '?';")

(define put-list-query "INSERT INTO lists (list_id, item) VALUES ('?', '?');")

(define put-set-query "INSERT INTO sets (set_id, item) VALUES ('?', '?');")

(define put-hash-query "INSERT INTO hashes (hash_id, key, value) VALUES ('?', '?', '?');")

(define put-vocab-query "INSERT INTO vocabularies (vocab_name, term) VALUES ('?', '?');")

(define last-list-id-query "SELECT DISTINCT list_id FROM lists ORDER BY list_id DESC LIMIT 1;")

(define last-set-id-query "SELECT DISTINCT set_id FROM sets ORDER BY set_id DESC LIMIT 1;")

(define last-hash-id-query "SELECT DISTINCT hash_id FROM hashes ORDER BY hash_id DESC LIMIT 1;")

;;; ========================================================================
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define (next-struct-id db struct-type)
  (let* ((qstring
           (case struct-type
             ((list) last-list-id-query)
             ((set) last-set-id-query)
             ((hash) last-hash-id-query)
             (else (error "No query for this struct type."))))
         (do-query
           (lambda () (query fetch-row (sql/transient db qstring))))
         (row
           (do-query))
         (last-id-str
           (and (pair? row) (car row)))
         (last-id
           (and last-id-str (string->number last-id-str)))
         (next-id
           (if last-id (+ last-id 1) (first-id struct-type)))
         (next-id-str
           (sprintf "~X" next-id)))
    (string-append "#x" (string-pad next-id-str 8 #\0))))

(define (next-list-id db) (next-struct-id db 'list))

(define (next-set-id db) (next-struct-id db 'set))

(define (next-hash-id db) (next-struct-id db 'hash))


(dbs:store-list
  (lambda (lst)
    (let* ((db (current-db))
           (st (sql db put-list-query))
           (id (next-list-id db)))
      (for-each
        (lambda (elt) (exec st id elt))
        lst))))
    
(dbs:store-set
  (lambda (set)
    (let* ((db (current-db))
           (st (sql db put-set-query))
           (id (next-set-id db)))
      (set-for-each
        (lambda (elt) (exec st id elt))
        set))))
    
(dbs:store-hash
  (lambda (alist)
    (let* ((db (current-db))
           (st (sql db put-hash-query))
           (id (next-hash-id db)))
      (for-each
        (lambda (elt) (exec st id (car elt) (cdr elt)))
        alist))))

(define (store-vocab voc_name terms)
  (let* ((db (current-db))
         (st (sql db put-vocab-query)))
    (for-each
      (lambda (term) (exec st voc_name term))
      terms)))

(dbs:retrieve-list
  (lambda (id)
    (let* ((db (current-db))
           (st (sql db get-list-query)))
      (query (map-rows car) st))))

(dbs:retrieve-set
  (lambda (id)
    (let* ((db (current-db))
           (st (sql db get-set-query)))
      (list->set
        (query (map-rows car) st)))))

(dbs:retrieve-hash
  (lambda (id)
    (let* ((db (current-db))
           (st (sql db get-hash-query)))
      (query
        (map-rows
          (lambda (row)
            (cons (car row) (cadr row))))
        st))))

(define (s->p+o s)
  (query fetch-all (sql/transient (current-db) s->p+o-query s)))

(define (p->s+o p)
  (query fetch-all (sql/transient (current-db) p->s+o-query p)))

(define (o->s+p o)
  (query fetch-all (sql/transient (current-db) o->s+p-query o)))

(define (s+p->o s p)
  (query fetch-all (sql/transient (current-db) s+p->o-query s p)))

(define (s+o->p s o)
  (query fetch-all (sql/transient (current-db) s+o->p-query s o)))

(define (p+o->s p o)
  (query fetch-all (sql/transient (current-db) p+o->s-query p o)))

(define (setup-db filename #!optional (force #f))
  (when (file-exists? filename)
    (if force
      (delete-file filename)
      (error "DB file already exists.")))
  (call-with-database
    filename
    (lambda (db)
      (for-each
        (lambda (qs) (exec (sql db qs)))
        setup-queries))))


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

