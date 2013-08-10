;;; cav-db.scm -- A sqlite3 database layer for Coq au Vin.
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
        (use srfi-19)

(define current-connection (make-parameter #f))

(define first-id (make-parameter (lambda (_) 0)))

(define db-file (make-parameter #f))

(define content-path (make-parameter #f))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  INITIAL SETUP  ---------------------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define setup-queries (make-parameter #f))

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define setup-db (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USERS, ROLES, AUTHENTICATION, & SESSIONS  ------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define add-role-query (make-parameter #f))

(define delete-role-query (make-parameter #f))

(define add-user-query (make-parameter #f))

(define user-exists-query (make-parameter #f))

(define get-user-info-query (make-parameter #f))

(define get-user-role-query (make-parameter #f))

(define update-password-query (make-parameter #f))

(define update-user-info-query (make-parameter #f))

(define delete-user-query (make-parameter #f))

(define user-blocked-query (make-parameter #f))

(define get-passhash-query (make-parameter #f))

(define bad-login-count-query (make-parameter #f))

(define add-bad-login-query (make-parameter #f))

(define clear-bad-logins-query (make-parameter #f))

(define block-user-query (make-parameter #f))

(define add-session-query (make-parameter #f))

(define refresh-session-query (make-parameter #f))

(define session-valid-query (make-parameter #f))

(define session-exists-query (make-parameter #f))

(define delete-session-query (make-parameter #f))

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define add-role (make-parameter #f))

(define delete-role (make-parameter #f))

(define add-user (make-parameter #f))

(define user-exists? (make-parameter #f))

(define get-user-info (make-parameter #f))

(define get-user-role (make-parameter #f))

(define update-password (make-parameter #f))

(define update-user-info (make-parameter #f))

(define delete-user (make-parameter #f))

(define can-login? (make-parameter #f))

(define get-passhash (make-parameter #f))

(define bad-login (make-parameter #f))

(define clear-bad-logins (make-parameter #f))

(define add-session (make-parameter #f))

(define refresh-session (make-parameter #f))

(define session-valid? (make-parameter #f))

(define session-exists? (make-parameter #f))

(define delete-session (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CREATING & EDITING CONTENT  --------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define create-article-query (make-parameter #f))

(define add-article-author-query (make-parameter #f))

(define add-article-tag-query (make-parameter #f))

(define add-article-category-query (make-parameter #f))

(define update-article-query (make-parameter #f))

(define delete-article-query (make-parameter #f))

(define delete-article-author-query (make-parameter #f))

(define delete-article-tag-query (make-parameter #f))

(define delete-article-category-query (make-parameter #f))

(define add-comment-query (make-parameter #f))

;; Should these be moved to the RETRIEVAL section?
(define comment-parent-query (make-parameter #f))

(define comment-children-query (make-parameter #f))

(define delete-comment-query (make-parameter #f)) 

(define nullify-comment-query (make-parameter #f)) 

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define create-article (make-parameter #f))

(define update-article (make-parameter #f))

(define delete-article (make-parameter #f))

(define add-comment (make-parameter #f))

(define comment-has-children? (make-parameter #f))

(define delete-comment (make-parameter #f))

(define delete-tree (make-parameter #f))

(define delete-thread (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RETRIEVING CONTENT  ----------------------------------------------

;;; ------  SQL Queries  ---------------------------------------------------

(define get-article-by-nodeid-query (make-parameter #f))

(define get-article-by-alias-query (make-parameter #f))

(define get-article-authors-query (make-parameter #f))

(define get-article-tags-query (make-parameter #f))

(define get-article-categories-query (make-parameter #f))

(define get-article-comment-ids-query (make-parameter #f))

(define get-comment-query (make-parameter #f))

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

; This only deals with the markdown original. Not sure where we should handle
; cached html.
(define get-article-body (make-parameter #f))

(define get-article-content (make-parameter #f))

(define get-article-by-nodeid (make-parameter #f))

(define get-article-by-alias (make-parameter #f))

(define get-article-comment-ids (make-parameter #f))

(define get-comment-thread (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

