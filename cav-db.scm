;;; cav-db.scm -- A sqlite3 database layer for Coq au Vin.
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module cav-db
        *
        (import scheme chicken)
        ; (import extras)
        ; (import files)
        ; (import utils)
        ; (import ports)
        ; (import data-structures)

        ; (use utf8)
        ; (use utf8-srfi-13)
        ; (use srfi-19)

(define current-connection (make-parameter #f))

(define first-id (make-parameter (lambda (_) 0)))


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USERS, ROLES, AUTHENTICATION, & SESSIONS  ------------------------

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

; This only deals with the markdown original. Not sure where we should handle
; cached html.
(define get-article-body (make-parameter #f))

(define get-article-content (make-parameter #f))

(define get-article-by-nodeid (make-parameter #f))

(define get-article-by-alias (make-parameter #f))

(define get-article-comment-ids (make-parameter #f))

(define get-comment-thread (make-parameter #f))

(define get-article-list (make-parameter #f))

(define get-articles-by-date (make-parameter #f))

(define get-meta-list (make-parameter #f))

(define get-ids-custom (make-parameter #f))

(define alias->node-id (make-parameter #f))

(define get-last-id (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  CONNECTING AND DISCONNECTING  ------------------------------------

(define connect (make-parameter #f))

(define disconnect (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

