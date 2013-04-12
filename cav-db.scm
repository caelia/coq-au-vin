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
        (use srfi-19)

(define current-db (make-parameter #f))

(define first-id (make-parameter (lambda (_) 0)))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  INITIAL SETUP  ---------------------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define setup-queries
  '(
#<<SQL
CREATE TABLE roles (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE
);
SQL

#<<SQL
CREATE TABLE tags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    tag TEXT NOT NULL UNIQUE
);
SQL

#<<SQL
CREATE TABLE categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category TEXT NOT NULL UNIQUE,
    description TEXT
);
SQL

#<<SQL
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    uname TEXT NOT NULL UNIQUE,
    passhash TEXT NOT NULL,
    email TEXT NOT NULL,
    role INTEGER REFERENCES roles(id) NOT NULL,
    display_name TEXT
);
SQL

#<<SQL
CREATE TABLE series (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL UNIQUE,
    description TEXT
);
SQL

#<<SQL
CREATE TABLE articles (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    series INTEGER REFERENCES series(id),
    series_pt INTEGER,
    subtitle TEXT,
    content TEXT NOT NULL,
    created_dt INTEGER NOT NULL,
    modified_dt INTEGER,
    version TEXT,
    teaser_len INTEGER
);
SQL
    
#<<SQL
CREATE TABLE articles_x_authors (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    article INTEGER REFERENCES articles(id),
    author INTEGER REFERENCES users(id) 
);
SQL

#<<SQL
CREATE TABLE articles_x_tags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    article INTEGER REFERENCES articles(id) NOT NULL,
    tag INTEGER REFERENCES tags(id) NOT NULL 
);
SQL

#<<SQL
CREATE TABLE articles_x_categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    article INTEGER REFERENCES articles(id) NOT NULL,
    tag INTEGER REFERENCES categories(id) NOT NULL 
);
SQL

#<<SQL
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    key TEXT UNIQUE NOT NULL,
    user INTEGER REFERENCES users(id),
    expires INTEGER NOT NULL
);
SQL
))

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

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

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USERS, ROLES, AUTHENTICATION, & SESSIONS  ------------------------

;;; ========================================================================
;;; ------------------------------------------------------------------------

(define add-role-query
  "INSERT INTO roles (name) VALUES (?);")

(define add-user-query
  "INSERT INTO users (uname, passhash, email, role, display_name) 
      SELECT ?, ?, ?, roles(id), ?
      WHERE roles(name) = ?;")

(define add-session-query
  "INSERT INTO sessions (key, user, expires)
      SELECT ?, users(id), ?
      WHERE users(uname) = ?;")

(define refresh-session-query
  "UPDATE sessions SET expires = ? WHERE key = ?;")

(define session-valid-query
  "SELECT id FROM sessions WHERE key = ? AND expires >= ?;")

(define session-exists-query
  "SELECT id FROM sessions WHERE key = ?;")

(define delete-session-query
  "DELETE FROM sessions WHERE key = ?;")

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

