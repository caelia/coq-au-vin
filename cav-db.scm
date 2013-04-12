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

(define current-connection (make-parameter #f))

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
    display_name TEXT,
    blocked_until INTEGER
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

#<<SQL
CREATE TABLE bad_logins (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user INTEGER REFERENCES users(id) NOT NULL,
    time INTEGER NOT NULL
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

;;; ------  SQL Queries  ---------------------------------------------------

(define add-role-query
  "INSERT INTO roles (name) VALUES (?);")

(define delete-role-query
  "DELETE FROM roles WHERE name = ?;")

(define add-user-query
  "INSERT INTO users (uname, passhash, email, role, display_name) 
      SELECT ?, ?, ?, id, ?
      FROM roles
      WHERE name = ?;")

(define user-exists-query
  "SELECT id FROM users WHERE uname = ?;")

(define get-user-info-query
  "SELECT email, role, display_name FROM users WHERE uname = ?;")

(define get-user-role-query
  "SELECT role FROM users WHERE uname = ?;")

(define update-password-query
  "UPDATE users SET passhash = ? WHERE uname = ?;")

(define update-user-info-query
  "UPDATE users SET email = ?, role = ?, display_name = ? WHERE uname = ?;")

(define delete-user-query
  "DELETE FROM users WHERE uname = ?;")

(define user-blocked-query
  "SELECT id FROM users WHERE uname = ? AND blocked_until > ?;")

(define get-passhash-query
  "SELECT passhash FROM users WHERE uname = ?;")

(define bad-login-count-query
  "SELECT count(id)
      FROM bad_logins
      WHERE user = users(id)
      AND users(uname) = ?;")

(define add-bad-login-query
  "INSERT INTO bad_logins (user, time)
      SELECT id, ?
      FROM users
      WHERE users(uname) = '?';")

(define clear-bad-logins-query
  "DELETE FROM bad_logins
      WHERE user = users(id) AND users(uname) = ?;")

(define block-user-query
  "UPDATE users SET blocked_until = ?
      WHERE uname = ?;")

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

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (add-role role-name)
  (let* ((conn (current-connection))
         (st (sql/transient conn add-role-query)))
    (exec st role-name)))

(define (delete-role role-name)
  (let* ((conn (current-connection))
         (st (sql/transient conn delete-role-query)))
    (exec st role-name)))

(define (add-user uname phash email role #!optional (disp-name '()))
  (let* ((conn (current-connection))
         (st (sql/transient conn add-user-query)))
    (exec st uname phash email disp-name role)))

(define (user-exists? uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn user-exists-query)))
    (query fetch-value st uname)))

(define (get-user-info uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn get-user-info-query)))
    (query fetch-alist st uname)))

(define (get-user-role uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn get-user-role-query)))
    (query fetch-value st uname)))

(define (update-password uname phash)
  (let* ((conn (current-connection))
         (st (sql/transient conn update-password-query)))
    (exec st phash uname)))

(define (update-user-info uname email role #!optional (disp-name '()))
  (let* ((conn (current-connection))
         (st (sql/transient conn update-user-info-query)))
    (exec st email role disp-name uname)))

(define (delete-user uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn delete-user-query)))
    (exec st uname)))

(define (can-login? uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn user-blocked-query)))
    (not (query fetch-value st uname (current-seconds)))))

(define (get-passhash uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn user-blocked-query)))
    (query fetch-value st)))

(define (bad-login uname)
  (let* ((conn (current-connection))
         (s-count (sql/transient conn bad-login-count-query))
         (s-add (sql/transient conn add-bad-login-query))
         (s-clear (sql/transient conn clear-bad-logins-query))
         (s-block (sql/transient conn block-user-query)))
    (let ((count (query fetch-value s-count)))
      (printf "bad login count: ~A\n" count)
      (if (and count (> count 1))
        (begin
          (exec s-clear uname)
          (exec s-block (+ current-seconds 120) uname))
        (exec s-add (current-seconds) uname)))))

(define (clear-bad-logins uname)
  (let* ((conn (current-connection))
         (st (sql/transient conn clear-bad-logins-query)))
    (exec st uname)))

(define (add-session key uname expires)
  (let* ((conn (current-connection))
         (st (sql/transient conn add-session-query)))
    (exec st key expires uname)))

(define (refresh-session key expires)
  (let* ((conn (current-connection))
         (st (sql/transient conn refresh-session-query)))
    (exec st expires key)))

(define (session-valid? key)
  (let* ((conn (current-connection))
         (st (sql/transient conn session-valid-query)))
    (query fetch-value st key (current-seconds))))

(define (session-exists? key)
  (let* ((conn (current-connection))
         (st (sql/transient conn session-exists-query)))
    (query fetch-value st key)))

(define (delete-session key)
  (let* ((conn (current-connection))
         (st (sql/transient conn delete-session-query)))
    (exec st key)))

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

