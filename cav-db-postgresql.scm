;;; cav-db-postgresql.scm -- A PostgreSQL database layer for Coq au Vin.
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module cav-db
        *
        (import scheme chicken)
        (import extras)
        (import files)
        (import utils)
        (import ports)
        (import data-structures)

        (use utf8)
        (use utf8-srfi-13)
        (use sql-de-lite)
        (use srfi-19)

(define current-connection (make-parameter #f))

(define first-id (make-parameter (lambda (_) 0)))

(define db-file (make-parameter #f))

(define content-path (make-parameter #f))

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
    node_id TEXT NOT NULL UNIQUE,
    title TEXT NOT NULL,
    series INTEGER REFERENCES series(id),
    series_pt INTEGER,
    subtitle TEXT,
    created_dt INTEGER NOT NULL,
    modified_dt INTEGER,
    version TEXT,
    teaser_len INTEGER,
    alias TEXT
);
SQL

#<<SQL
CREATE TABLE comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    node_id TEXT NOT NULL UNIQUE,
    article INTEGER REFERENCES articles(id) NOT NULL,
    author INTEGER REFERENCES users(id) NOT NULL,
    created_dt INTEGER NOT NULL,
    text TEXT NOT NULL,
    parent INTEGER
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
    category INTEGER REFERENCES categories(id) NOT NULL 
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



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CREATING & EDITING CONTENT  --------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define create-article-query
#<<SQL
INSERT INTO articles (node_id, title, series, series_pt, subtitle, created_dt, teaser_len, alias)
  SELECT ?, ?, ?, ?, ?, strftime('%s', 'now', 'localtime'), ?, ?;
SQL
)

(define add-article-author-query
#<<SQL
INSERT INTO articles_x_authors (article, author)
  SELECT articles.id, authors.id
  FROM articles, authors
  WHERE articles(node_id) = ? AND users(uname) = ?;
SQL
)

(define add-article-tag-query
#<<SQL
INSERT INTO articles_x_tags (article, tag)
  SELECT articles.id, tags.id
  FROM articles, tags
  WHERE articles(node_id) = ? AND tags(tag) = ?;
SQL
)

(define add-article-category-query
#<<SQL
INSERT INTO articles_x_categories (article, category)
  SELECT articles.id, categories.id
  FROM articles, categories
  WHERE articles(node_id) = ? AND categories(category) = ?;
SQL
)

(define update-article-query
#<<SQL
UPDATE articles
SET title = ?, series = ?, series_pt = ?, subtitle = ?, teaser_len = ?, alias = ?
WHERE node_id = ?;
SQL
)

(define delete-article-query
#<<SQL
DELETE FROM articles WHERE node_id = ?;
SQL
)

(define delete-article-author-query
#<<SQL
DELETE FROM articles_x_authors
WHERE article = articles(id) AND articles(id) = ?;
SQL
)

(define delete-article-tag-query
#<<SQL
DELETE FROM articles_x_tag
WHERE article = articles(id) AND articles(id) = ?;
SQL
)

(define delete-article-category-query
#<<SQL
DELETE FROM articles_x_categories
WHERE article = articles(id) AND articles(id) = ?;
SQL
)

(define add-comment-query
#<<SQL
INSERT INTO comments (node-id, article, author, created_dt, text, parent)
SELECT ?, articles(id), authors(id), strftime('%s', 'now', 'localtime'), ?, ?
WHERE articles(node_id) = ? AND users(uname) = ?;
SQL
)

;; Should these be moved to the RETRIEVAL section?
(define comment-parent-query
  "SELECT parent FROM comments WHERE node_id = ?;")

(define comment-children-query
  "SELECT node_id FROM comments WHERE parent = ?;")

(define delete-comment-query 
  "DELETE FROM comments WHERE node_id = ?;")

(define nullify-comment-query 
#<<SQL
UPDATE comments
SET author = 'nobody', text = 'This comment has been deleted'
WHERE node_id = ?;
SQL
)

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (create-article node-id title author/s
                        #!key (series '()) (series-pt '()) (subtitle '())
                        (teaser-len '()) (alias '()) (tags '()) (categories '()))
  (let* ((authors (if (list? author/s) author/s (list author/s)))
         (conn (current-connection))
         (st-art (sql/transient conn create-article-query))
         (st-auth (sql conn add-article-author-query))
         (st-tag (sql conn add-article-tag-query))
         (st-cat (sql conn add-article-category-query)))
    (exec st-art node-id title series series-pt subtitle teaser-len alias)
    (for-each
      (lambda (auth) (exec st-auth node-id auth))
      authors)
    (for-each
      (lambda (tag) (exec st-tag node-id tag))
      tags)
    (for-each
      (lambda (cat) (exec st-cat node-id cat))
      categories)))

(define (update-article node-id #!key (title '()) (series '()) (series-pt '()) (subtitle '())
                        (teaser-len '()) (alias '()) (tags '()) (categories '()))
  (let* ((conn (current-connection))
         (st-art (sql/transient conn update-article-query))
         (st-tag (sql conn add-article-tag-query))
         (st-cat (sql conn add-article-category-query)))
    (exec st-art title series series-pt subtitle teaser-len alias node-id)
    (for-each
      (lambda (tag) (exec st-tag node-id tag))
      tags)
    (for-each
      (lambda (cat) (exec st-cat node-id cat))
      categories)))

(define (delete-article node-id)
  (let* ((conn (current-connection))
         (st-art (sql/transient conn delete-article-query))
         (st-auth (sql conn delete-article-author-query))
         (st-tag (sql conn delete-article-tag-query))
         (st-cat (sql conn delete-article-category-query)))
    (for-each
      (lambda (st) (exec st node-id))
      `(,st-art ,st-auth ,st-tag ,st-cat))))

(define (add-comment node-id article author text #!optional (parent #f))
  (let* ((conn (current-connection))
         (st (sql/transient conn add-comment-query)))
    (exec st text parent article author)))

(define (comment-has-children? node-id)
  (let* ((conn (current-connection))
         (st (sql/transient conn comment-children-query)))
    (query fetch-value st node-id)))

(define (delete-comment node-id)
  (let* ((conn (current-connection))
         (st-children? (sql/transient conn comment-children-query))
         (st-nullify (sql/transient conn nullify-comment-query))
         (st-delete (sql/transient conn delete-comment-query))
         (has-children (query fetch-value st-children? node-id)))
    (if has-children
      (exec st-nullify node-id)
      (exec st-delete node-id))))

(define (delete-tree node-id st-children st-parent st-delete)
  (let ((children (query fetch-all st-children node-id))
        (parent (query fetch-value st-parent node-id)))
    (if (null? children)
      (begin
        (exec st-delete node-id)
        (unless (null? parent)
          (delete-tree parent st-children st-parent st-delete)))
      (for-each
        (lambda (child) (delete-tree child st-children st-parent st-delete))
        children))))

(define (delete-thread node-id)
  (let* ((conn (current-connection))
         (st-children (sql conn comment-children-query))
         (st-parent (sql conn comment-parent-query))
         (st-delete (sql conn delete-comment-query)))
    (delete-tree node-id st-children st-parent st-delete)))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RETRIEVING CONTENT  ----------------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define get-article-by-nodeid-query
#<<SQL
SELECT title, series, series_pt, subtitle, created_dt, teaser_len
FROM articles
WHERE node_id = ?;
SQL
)

(define get-article-by-alias-query
#<<SQL
SELECT node_id, title, series, series_pt, subtitle, created_dt, teaser_len
FROM articles
WHERE alias = ?;
SQL
)

(define get-article-authors-query
#<<SQL
SELECT uname, display_name
FROM users, articles, articles_x_authors
WHERE users.id = articles_x_authors.author
AND articles_x_authors.article = articles.id
AND articles.node_id = ?;
SQL
)

(define get-article-tags-query
#<<SQL
SELECT tag FROM tags
WHERE tags(id) = articles_x_tags(tag)
AND articles_x_tags(article) = articles(id)
AND articles(node_id) = ?;
SQL
)

(define get-article-categories-query
#<<SQL
SELECT category FROM categories
WHERE categories(id) = articles_x_categories(category)
AND articles_x_categories(article) = articles(id)
AND articles(node_id) = ?;
SQL
)

(define get-article-comment-ids-query
#<<SQL
SELECT node_id FROM comments
WHERE article = articles(id) AND articles(node_id) = ?
AND parent = NULL;
SQL
)

(define get-comment-query
  "SELECT author, created_dt, text WHERE node_id = ?;")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

; This only deals with the markdown original. Not sure where we should handle
; cached html.
; (define (get-article-body path #!optional (out #f))
(define (get-article-body path)
  (let ((body-path (make-pathname path "body")))
    (with-output-to-string
      (lambda ()
        (with-input-from-file
          body-path
          (lambda ()
            (display (read-all))))))))

; (define (get-article-content node-id #!optional (out #f))
(define (get-article-content node-id)
  (let ((article-path (make-pathname (content-path) node-id))) 
    `((body . ,(get-article-body article-path)))))

(define (get-article-by-nodeid node-id)
  (let* ((conn (current-connection))
         (st-data (sql/transient conn get-article-by-nodeid-query))
         (st-auth (sql/transient conn get-article-authors-query))
         (article-data (query fetch-alist st-data node-id))
         (authors (query fetch st-auth node-id))
         (content (get-article-content node-id)))
    (cons
      (cons 'content content)
      (cons
        (cons 'authors authors)
        article-data))))

(define (get-article-by-alias alias)
  (let* ((conn (current-connection))
         (st-data (sql/transient conn get-article-by-alias-query))
         (st-auth (sql/transient conn get-article-authors-query))
         (article-data (query fetch-alist st-data alias))
         (node-id (alist-ref 'node_id article-data))
         (authors (query fetch-all st-auth node-id))
         (content (get-article-content node-id)))
    (cons
      (cons 'content content)
      (cons
        (cons 'authors authors)
        article-data))))

(define (get-article-comment-ids node-id)
  (let* ((conn (current-connection))
         (st (sql/transient conn get-article-comment-ids-query)))
    (query fetch-all st node-id)))

(define (get-comment-thread node-id #!optional (depth #f))
  (let* ((conn (current-connection))
         (st-kids (sql conn add-comment-query))
         (st-content (sql conn get-comment-query)))
    (let loop ((id node-id))
      (let* ((content (query fetch-alist st-content id))
             (kid-ids (query fetch-all st-kids id)))
        (if (null? kid-ids)
          content
          (append content `(children . ,(map loop kid-ids))))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

