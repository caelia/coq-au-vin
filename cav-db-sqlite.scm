;;; cav-db-sqlite.scm -- A SQLite3 database layer for Coq au Vin.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

; Just for initial development phase
; (load "cav-db.so")

(module cav-db-sqlite
        *
        ; (activate-sqlite open-database close-database)

        (import scheme chicken)
        (import extras)
        (import files)
        (import utils)
        (import ports)
        (import posix)
        (import data-structures)

        (use utf8)
        (use utf8-srfi-13)
        (use (prefix sql-de-lite sd:))
        ; (use srfi-19) ; I think we're using all posix date/time stuff now
        (use sets)

        (use cav-db)
        ; Just for initial development phase
        ; (require-library cav-db)
        ; (import cav-db)

; These 4 params probably only needed in cav-db
; (define current-connection (make-parameter #f))

; (define first-id (make-parameter (lambda (_) 0)))

; (define db-file (make-parameter #f))

; (define content-path (make-parameter #f))

(define open-database sd:open-database)
(define close-database sd:close-database)

(define (falsify alist)
  (map
    (lambda (pair)
      (let ((key (car pair))
            (val (cdr pair)))
        (cond
          ((null? val) `(,key . #f))
          ((list? val) `(,key . ,(falsify val)))
          (else pair))))
    alist))

(define (cull-null alist)
  (foldl
    (lambda (prev pair)
      (let ((key (car pair))
            (val (cdr pair)))
        (cond
          ((null? val) prev)
          ((list? val) (cons `(,key . ,(cull-null val)) prev))
          (else (cons pair prev)))))
    '()
    alist))

(define (leap-year? y)
  (let ((multiple-of? (lambda (m n) (= (modulo m n) 0))))
    (or (multiple-of? y 400)
        (and (multiple-of? y 4)
             (not (multiple-of? y 100))))))

(define (next-day time)
  (let* ((y* (vector-ref time 5))
         (y (+ y* 1900))
         (m* (vector-ref time 4))
         (m (+ m* 1))
         (d (vector-ref time 3))
         (feb-last (if (leap-year? y) 29 28))
         (time-out (make-vector 10))
         (bump-month
           (lambda ()
             (vector-set! time-out 4 m)
             (vector-set! time-out 3 1))))
    (vector-copy! time time-out)
    (cond
      ((or (> m 12) (< m 1) (> d 31) (< d 1))
       (error "Invalid date."))
      ((and (member m '(4 6 9 11))
            (> d 30))
       (error "Invalid date."))
      ((and (= m 2) (> d feb-last))
       (error "Invalid date."))
      ((and (= m 12) (= d 31))
       (vector-set! time-out 5 (- y 1899))
       (vector-set! time-out 4 0)
       (vector-set! time-out 3 1))
      ((and (member m '(1 3 5 7 8 10)) (= d 31))
       (bump-month))
      ((and (member m '(4 6 9 11)) (= d 30))
       (bump-month))
      ((and (= m 2) (= d feb-last))
       (bump-month))
      (else
        (vector-set! time-out 3 (+ d 1))))
    time-out))

(define (iso-date->time iso)
  (string->time iso "%F"))

(define (ymd->time y m d)
  (iso-date->time (sprintf "~A-~A-~A" y m d)))

(define (get-date-start-end start)
  (let* ((tstart (iso-date->time start))
         (tend (next-day tstart))
         (starts (local-time->seconds tstart))
         (ends (local-time->seconds tend)))
    (values starts ends)))

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
    sticky INTEGER DEFAULT 0,
    sticky_until INTEGER,
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

(define (%setup-db filename #!optional (force #f))
  (when (file-exists? filename)
    (if force
      (delete-file filename)
      (error "DB file already exists.")))
  (sd:call-with-database
    filename
    (lambda (db)
      (for-each
        (lambda (qs) (sd:exec (sd:sql db qs)))
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

(define (%add-role role-name)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn add-role-query)))
    (sd:exec st role-name)))

(define (%delete-role role-name)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn delete-role-query)))
    (sd:exec st role-name)))

(define (%add-user uname phash email role #!optional (disp-name '()))
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn add-user-query)))
    (sd:exec st uname phash email disp-name role)))

(define (%user-exists? uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn user-exists-query)))
    (sd:query sd:fetch-value st uname)))

(define (%get-user-info uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn get-user-info-query)))
    (sd:query sd:fetch-alist st uname)))

(define (%get-user-role uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn get-user-role-query)))
    (sd:query sd:fetch-value st uname)))

(define (%update-password uname phash)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn update-password-query)))
    (sd:exec st phash uname)))

(define (%update-user-info uname email role #!optional (disp-name '()))
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn update-user-info-query)))
    (sd:exec st email role disp-name uname)))

(define (%delete-user uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn delete-user-query)))
    (sd:exec st uname)))

(define (%can-login? uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn user-blocked-query)))
    (not (sd:query sd:fetch-value st uname (current-seconds)))))

(define (%get-passhash uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn user-blocked-query)))
    (sd:query sd:fetch-value st)))

(define (%bad-login uname)
  (let* ((conn (current-connection))
         (s-count (sd:sql/transient conn bad-login-count-query))
         (s-add (sd:sql/transient conn add-bad-login-query))
         (s-clear (sd:sql/transient conn clear-bad-logins-query))
         (s-block (sd:sql/transient conn block-user-query)))
    (let ((count (sd:query sd:fetch-value s-count)))
      (printf "bad login count: ~A\n" count)
      (if (and count (> count 1))
        (begin
          (sd:exec s-clear uname)
          (sd:exec s-block (+ current-seconds 120) uname))
        (sd:exec s-add (current-seconds) uname)))))

(define (%clear-bad-logins uname)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn clear-bad-logins-query)))
    (sd:exec st uname)))

(define (%add-session key uname expires)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn add-session-query)))
    (sd:exec st key expires uname)))

(define (%refresh-session key expires)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn refresh-session-query)))
    (sd:exec st expires key)))

(define (%session-valid? key)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn session-valid-query)))
    (sd:query sd:fetch-value st key (current-seconds))))

(define (%session-exists? key)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn session-exists-query)))
    (sd:query sd:fetch-value st key)))

(define (%delete-session key)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn delete-session-query)))
    (sd:exec st key)))

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

(define (%create-article node-id title author/s
                         #!key (series '()) (series-pt '()) (subtitle '())
                         (teaser-len '()) (alias '()) (tags '()) (categories '()))
  (let* ((authors (if (list? author/s) author/s (list author/s)))
         (conn (current-connection))
         (st-art (sd:sql/transient conn create-article-query))
         (st-auth (sd:sql conn add-article-author-query))
         (st-tag (sd:sql conn add-article-tag-query))
         (st-cat (sd:sql conn add-article-category-query)))
    (sd:exec st-art node-id title series series-pt subtitle teaser-len alias)
    (for-each
      (lambda (auth) (sd:exec st-auth node-id auth))
      authors)
    (for-each
      (lambda (tag) (sd:exec st-tag node-id tag))
      tags)
    (for-each
      (lambda (cat) (sd:exec st-cat node-id cat))
      categories)))

(define (%update-article node-id #!key (title '()) (series '()) (series-pt '()) (subtitle '())
                        (teaser-len '()) (alias '()) (tags '()) (categories '()))
  (let* ((conn (current-connection))
         (st-art (sd:sql/transient conn update-article-query))
         (st-tag (sd:sql conn add-article-tag-query))
         (st-cat (sd:sql conn add-article-category-query)))
    (sd:exec st-art title series series-pt subtitle teaser-len alias node-id)
    (for-each
      (lambda (tag) (sd:exec st-tag node-id tag))
      tags)
    (for-each
      (lambda (cat) (sd:exec st-cat node-id cat))
      categories)))

(define (%delete-article node-id)
  (let* ((conn (current-connection))
         (st-art (sd:sql/transient conn delete-article-query))
         (st-auth (sd:sql conn delete-article-author-query))
         (st-tag (sd:sql conn delete-article-tag-query))
         (st-cat (sd:sql conn delete-article-category-query)))
    (for-each
      (lambda (st) (sd:exec st node-id))
      `(,st-art ,st-auth ,st-tag ,st-cat))))

(define (%add-comment node-id article author text #!optional (parent #f))
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn add-comment-query)))
    (sd:exec st text parent article author)))

(define (%comment-has-children? node-id)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn comment-children-query)))
    (sd:query sd:fetch-value st node-id)))

(define (%delete-comment node-id)
  (let* ((conn (current-connection))
         (st-children? (sd:sql/transient conn comment-children-query))
         (st-nullify (sd:sql/transient conn nullify-comment-query))
         (st-delete (sd:sql/transient conn delete-comment-query))
         (has-children (sd:query sd:fetch-value st-children? node-id)))
    (if has-children
      (sd:exec st-nullify node-id)
      (sd:exec st-delete node-id))))

(define (%delete-tree node-id st-children st-parent st-delete)
  (let ((children (sd:query sd:fetch-all st-children node-id))
        (parent (sd:query sd:fetch-value st-parent node-id)))
    (if (null? children)
      (begin
        (sd:exec st-delete node-id)
        (unless (null? parent)
          (delete-tree parent st-children st-parent st-delete)))
      (for-each
        (lambda (child) (delete-tree child st-children st-parent st-delete))
        children))))

(define (%delete-thread node-id)
  (let* ((conn (current-connection))
         (st-children (sd:sql conn comment-children-query))
         (st-parent (sd:sql conn comment-parent-query))
         (st-delete (sd:sql conn delete-comment-query)))
    (delete-tree node-id st-children st-parent st-delete)))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RETRIEVING CONTENT  ----------------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define get-article-by-nodeid-query
#<<SQL
SELECT title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles
WHERE node_id = ?;
SQL
)

(define get-article-by-alias-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles
WHERE alias = ?;
SQL
)

(define get-article-series-query
#<<SQL
SELECT series.title as series, series_pt
FROM articles, series
WHERE node_id = ? AND series.id = articles.series;
SQL
)

(define get-article-categories-query
#<<SQL
SELECT categories.category
FROM articles, articles_x_categories, categories
WHERE node_id = ? AND articles_x_categories.article = articles.id AND articles_x_categories.category = categories.id ;
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
SELECT tags.tag FROM tags, articles_x_tags, articles
WHERE tags.id = articles_x_tags.tag
AND articles_x_tags.article = articles.id
AND articles.node_id = ?;
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

(define get-article-count-query
  "SELECT count(*) FROM articles;")

(define get-article-with-tag-count-query
#<<SQL
SELECT count(*) FROM articles, articles_x_tags, tags
WHERE articles_x_tags.article = articles.id AND articles_x_tags.tag = tags.id AND tags.tag = ?
SQL
)

(define get-article-by-author-count-query
#<<SQL
SELECT count(*) FROM articles, articles_x_authors, users
WHERE articles_x_authors.article = articles.id AND articles_x_authors.author = users.id AND users.uname = ?
SQL
)

(define get-series-article-count-query
#<<SQL
SELECT count(*) FROM articles, series
WHERE articles.series = series.id AND series.title = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-article-with-category-count-query
#<<SQL
SELECT count(*) FROM articles, articles_x_categories, categories
WHERE articles_x_categories.article = articles.id
  AND articles_x_categories.category = categories.id
  AND categories.category = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-article-in-date-range-count-query
#<<SQL
SELECT count(*) FROM articles
WHERE created_dt >= ? AND created_dt < ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-ids-all-query
#<<SQL
SELECT node_id FROM articles
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-ids-with-tag-query
#<<SQL
SELECT node_id FROM articles, articles_x_tags, tags
WHERE articles_x_tags.article = articles.id AND articles_x_tags.tag = tags.id AND tags.tag = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-ids-by-author-query
#<<SQL
SELECT node_id FROM articles, articles_x_authors, users
WHERE articles_x_authors.article = articles.id AND articles_x_authors.author = users.id AND users.uname = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-series-ids-query
#<<SQL
SELECT node_id FROM articles, series
WHERE articles.series = series.id AND series.title = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-ids-with-category-query
#<<SQL
SELECT node_id FROM articles, articles_x_categories, categories
WHERE articles_x_categories.article = articles.id
  AND articles_x_categories.category = categories.id
  AND categories.category = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-ids-in-date-range-query
#<<SQL
SELECT node_id FROM articles
WHERE created_dt >= ? AND created_dt < ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-articles-all-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-articles-with-tag-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles, articles_x_tags, tags
WHERE articles_x_tags.article = articles.id AND articles_x_tags.tag = tags.id AND tags.tag = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-articles-by-author-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles, articles_x_authors, users
WHERE articles_x_authors.article = articles.id AND articles_x_authors.author = users.id AND users.uname = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-series-articles-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles, series
WHERE articles.series = series.id AND series.title = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-articles-with-category-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles, articles_x_categories, categories
WHERE articles_x_categories.article = articles.id
  AND articles_x_categories.category = categories.id
  AND categories.category = ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-articles-in-date-range-query
#<<SQL
SELECT node_id, title, subtitle, created_dt, teaser_len, sticky, sticky_until
FROM articles
WHERE created_dt >= ? AND created_dt < ?
ORDER BY created_dt DESC
LIMIT ? OFFSET ?;
SQL
)

(define get-series-list-query "SELECT title FROM series;")

(define get-tag-list-query "SELECT tag FROM tags;")

(define get-category-list-query "SELECT category FROM categories;")

(define get-author-list-query
  "SELECT DISTINCT uname, display_name FROM users, articles_x_authors WHERE articles_x_authors.author = users.id;")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

; This only deals with the markdown original. Not sure where we should handle
; cached html.
; (define (get-article-body path #!optional (out #f))
(define (%get-article-body path)
  (let ((body-path (make-pathname path "body")))
    (with-output-to-string
      (lambda ()
        (with-input-from-file
          body-path
          (lambda ()
            (display (read-all))))))))

; (define (get-article-content node-id #!optional (out #f))
(define (%get-article-content node-id)
  (let ((article-path (make-pathname (content-path) node-id))) 
    `((body . ,(%get-article-body article-path)))))

(define (statement-generator/single conn queries)
  (lambda (sym)
    (sd:sql/transient conn (alist-ref sym queries))))

(define (statement-generator/multi conn queries)
  (let ((statements
          (map
            (lambda (sym+query)
              (let ((sym (car sym+query)) (query (cdr sym+query)))
                (cons sym (sd:sql conn query))))
            queries)))
    (lambda (sym)
      (alist-ref sym statements))))

(define common-data-queries
  `((series . ,get-article-series-query)
    (categories . ,get-article-categories-query)
    (authors . ,get-article-authors-query)
    (tags . ,get-article-tags-query)))

(define (common-data-stgen/single conn)
  (statement-generator/single conn common-data-queries))

(define (common-data-stgen/multi conn)
  (statement-generator/multi conn common-data-queries))

(define (%get-article-common-data stgen node-id)
  (let* ((st-series (stgen 'series))
         (st-categories (stgen 'categories))
         (st-auth (stgen 'authors))
         (st-tags (stgen 'tags))
         (series (sd:query sd:fetch-alist st-series node-id))
         (categories (sd:query sd:fetch-all st-categories node-id))
         (authors (sd:query sd:fetch-alists st-auth node-id))
         (tags (sd:query sd:fetch-all st-tags node-id))
         (content (%get-article-content node-id)))
    (append
      `((content . ,content)
        (authors . ,(falsify authors))
        (tags . ,(flatten tags))
        (categories . ,(flatten categories)))
      series)))

(define (%get-article-by-nodeid node-id)
  (let* ((conn (current-connection))
         (st-data (sd:sql/transient conn get-article-by-nodeid-query))
         (article-data (sd:query sd:fetch-alist st-data node-id))
         (stgen (common-data-stgen/single conn)))
    (append
      (falsify article-data)
      (%get-article-common-data stgen node-id))))

(define (%get-article-by-alias alias)
  (let* ((conn (current-connection))
         (st-data (sd:sql/transient conn get-article-by-alias-query))
         (article-data (sd:query sd:fetch-alist st-data alias))
         (node-id (alist-ref 'node_id article-data))
         (stgen (common-data-stgen/single conn)))
    (append
      (falsify article-data)
      (%get-article-common-data stgen node-id))))

(define (%get-article-comment-ids node-id)
  (let* ((conn (current-connection))
         (st (sd:sql/transient conn get-article-comment-ids-query)))
    (sd:query sd:fetch-all st node-id)))

(define (%get-comment-thread node-id #!optional (depth #f))
  (let* ((conn (current-connection))
         (st-kids (sd:sql conn add-comment-query))
         (st-content (sd:sql conn get-comment-query)))
    (let loop ((id node-id))
      (let* ((content (sd:query sd:fetch-alist st-content id))
             (kid-ids (sd:query sd:fetch-all st-kids id)))
        (if (null? kid-ids)
          content
          (append content `(children . ,(map loop kid-ids))))))))

(define (%prepare-get-articles-queries criterion)
  (if (list? criterion)
    (case (car criterion)
      ((tag)
       (values
         get-article-with-tag-count-query
         get-articles-with-tag-query
         (cadr criterion) #f))
      ((author)
       (values
         get-article-by-author-count-query
         get-articles-by-author-query
         (cadr criterion) #f))
      ((series)
       (values
         get-series-article-count-query
         get-series-articles-query
         (cadr criterion) #f))
      ((category)
       (values
         get-article-with-category-count-query
         get-articles-with-category-query
         (cadr criterion) #f))
      ((date-range)
       (values
         get-article-in-date-range-count-query
         get-articles-in-date-range-query
         (cadr criterion) (caddr criterion))))
    (values
      get-article-count-query
      get-articles-all-query
      #f #f)))

(define (%get-article-list-0 st-count st-data limit offset)
  (values (car (sd:query sd:fetch st-count))
          (sd:query sd:fetch-alists st-data limit offset)))

(define (%get-article-list-1 st-count st-data param limit offset)
  (values (car (sd:query sd:fetch st-count param))
          (sd:query sd:fetch-alists st-data param limit offset)))

(define (%get-article-list-2 st-count st-data param1 param2 limit offset)
  (values (car (sd:query sd:fetch st-count param1 param2))
          (sd:query sd:fetch-alists st-data param1 param2 limit offset)))

(define (%get-article-list criterion limit offset mk-teaser)
  (let-values (((qcount qdata param1 param2)
                (%prepare-get-articles-queries criterion)))
    (let* ((conn (current-connection))
           (st-count (sd:sql/transient conn qcount))
           (st-data (sd:sql/transient conn qdata))
           (stgen (common-data-stgen/multi conn))
           ;; FIXME -- hmmm ... probably want to enable add additional stuff
           ;; for the future.
           (process-content
             (lambda (cont)
               (list (cons 'teaser (mk-teaser (alist-ref 'body cont)))))))
      (let-values (((count data*)
                    (cond
                      ((and param1 param2)
                       (%get-article-list-2 st-count st-data param1 param2
                                        limit offset))
                      (param1
                       (%get-article-list-1 st-count st-data param1 
                                        limit offset))
                      (else
                       (%get-article-list-0 st-count st-data limit offset)))))
      (let loop ((data-in data*)
                 (data-out '()))
        (if (null? data-in)
          (values count (reverse data-out))
          (let* ((datum (car data-in))
                 (node-id (alist-ref 'node_id datum))
                 (common* (%get-article-common-data stgen node-id))
                 (content (alist-ref 'content common*))
                 (common (alist-update 'content (process-content content) common*)) 
                 (result (append common (falsify datum))))
            (loop (cdr data-in) (cons result data-out)))))))))

(define (%get-articles-all #!key (limit 10) (offset 0) (mk-teaser identity))
  (%get-article-list 'all limit offset mk-teaser))

(define (%get-articles-with-tag tag #!key (limit 10) (offset 0) (mk-teaser identity))
  (%get-article-list `(tag ,tag) limit offset mk-teaser))

(define (%get-articles-by-author author #!key (limit 10) (offset 0) (mk-teaser identity))
  (%get-article-list `(author ,author) limit offset mk-teaser))

(define (%get-articles-in-series series #!key (limit 10) (offset 0) (mk-teaser identity))
  (%get-article-list `(series ,series) limit offset mk-teaser))

(define (%get-articles-with-category category #!key (limit 10) (offset 0) (mk-teaser identity))
  (%get-article-list `(category ,category) limit offset mk-teaser))

(define (%get-articles-by-date date #!key (limit 10) (offset 0) (mk-teaser identity))
  (let-values (((start end) (get-date-start-end date)))
    (%get-article-list `(date-range ,start ,end) limit offset mk-teaser)))

(define (%get-meta-list subject)
  (let* ((qstr
           (case subject
             ((series) get-series-list-query)
             ((tags) get-tag-list-query)
             ((categories) get-category-list-query)
             ((authors) get-author-list-query)
             (else (error "Invalid subject for %get-meta-list."))))
         (conn (current-connection))
         (st (sd:sql/transient conn qstr)))
    (sd:query sd:fetch-all st)))


(define (%get-ids-custom criterion)
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define (enable-sqlite)
  (setup-db %setup-db)
  (add-role %add-role)
  (delete-role %delete-role)
  (add-user %add-user)
  (user-exists? %user-exists?)
  (get-user-info %get-user-info)
  (get-user-role %get-user-role)
  (update-password %update-password)
  (update-user-info %update-user-info)
  (delete-user %delete-user)
  (can-login? %can-login?)
  (get-passhash %get-passhash)
  (bad-login %bad-login)
  (clear-bad-logins %clear-bad-logins)
  (add-session %add-session)
  (refresh-session %refresh-session)
  (session-valid? %session-valid?)
  (session-exists? %session-exists?)
  (delete-session %delete-session)
  (create-article %create-article)
  (update-article %update-article)
  (delete-article %delete-article)
  (add-comment %add-comment)
  (comment-has-children? %comment-has-children?)
  (delete-comment %delete-comment)
  (delete-tree %delete-tree)
  (delete-thread %delete-thread)
  (get-article-body %get-article-body)
  (get-article-content %get-article-content)
  (get-article-by-nodeid %get-article-by-nodeid)
  (get-article-by-alias %get-article-by-alias)
  (get-article-comment-ids %get-article-comment-ids)
  (get-comment-thread %get-comment-thread)
  (get-article-list %get-article-list)
  (get-articles-by-date %get-articles-by-date)
  (get-meta-list %get-meta-list)
  (get-ids-custom %get-ids-custom)
  #t)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
