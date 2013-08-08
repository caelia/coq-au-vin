;;; coq-au-vin.scm -- A simple blogging platform based on Chicken Scheme.
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(import scheme)
(import chicken)
(import files)
(import ports)

(use lowdown)
(use (prefix civet cvt:))
(use srfi-69)
; (use crypt)
;; FIXME: Need a better password hash!
(use simple-sha1)

(use spiffy)
(use intarweb)
(use matchable)

;; temporary
(load "cav-db.so")
(import (prefix cav-db db:))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))
 
(define (strip-html str)
  (let* ((in-tag #f)
         (enter-tag
           (lambda ()
             (if in-tag
               (set! in-tag (+ in-tag 1))
               (set! in-tag 1))))
         (leave-tag
           (lambda ()
             (cond
               ((not in-tag) #f)
               ((= in-tag 1) (set! in-tag #f))
               (else (set! in-tag (- in-tag 1)))))))
    (with-output-to-string
      (lambda ()
        (with-input-from-string
          str
          (lambda ()
            (let loop ((c (read-char)))
              (cond
                ((eof-object? c)
                 #f)
                ((char=? c #\<)
                 (enter-tag)
                 (loop (read-char)))
                ((char=? c #\>)
                 (leave-tag)
                 (loop (read-char)))
                (in-tag
                  (loop (read-char)))
                (else
                  (write-char c)
                  (loop (read-char)))))))))))

(define (escape-html str)
  (with-output-to-string
    (lambda ()
      (with-input-from-string
        str
        (lambda ()
          (let loop ((c (read-char)))
            (cond
              ((eof-object? c)
               #f)
              ((char=? c #\<)
               (display "&lt;")
               (loop (read-char)))
              ((char=? c #\>)
               (display "&gt;")
               (loop (read-char)))
              (else
                (write-char c)
                (loop (read-char))))))))))

(define (string->bool s)
  (let ((s (string-downcase s)))
    (cond
      ((string=? s "true") #t)
      ((string=? s "false") #t)
      (eprintf "'~A' does not represent a boolean value.\n"))))

; This is not absolutely foolproof, but I think it's close enough
(define (node-id? s)
  (and (= (string-length s) 8)
       (string->number (string-append "#x" s))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS  -----------------------------------------------

(define %blog-root% (make-parameter #f))

(define %default-teaser-length% (make-parameter 1024))

(define %config% (make-parameter (make-hash-table)))

(define %session-timeout% (make-parameter 900))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  RUNTIME CONFIGURATION  -------------------------------------------

(define (read-config-blog-data cfg params)
  (let loop ((params params))
    (if (null? params)
      cfg
      (let* ((param (car params))
             (key (car param))
             (val (cdr param)))
        (case key
          ((raw-html)
           (hash-table-set! cfg key (string->symbol val)))
          ((max-post-length max-comment-length teaser-length)
           (hash-table-set! cfg key (string->number val)))
          (else #f))
        (loop (cdr params))))))

(define (read-config-blog-interface cfg params)
  (let loop ((params params))
    (if (null? params)
      cfg
      (let* ((param (car params))
             (key (car param))
             (val (cdr param)))
        (case key
          ((use-javascipt inline-editing)
           (hash-table-set! cfg key (string->bool val)))
          (else #f))
        (loop (cdr params))))))

(define (read-config-blog-appearance cfg params)
  (let loop ((params params))
    (if (null? params)
      cfg
      (let* ((param (car params))
             (key (car param))
             (val (cdr param)))
        (case key
          ((layout theme)
           (hash-table-set! cfg key val))
          (else #f))
        (loop (cdr params))))))

(define (read-config-blog cfg params)
  (let loop ((params params))
    (if (null? params)
      cfg
      (let* ((param (car params))
             (key (car param))
             (val* (cdr param))
             (process-value
               (case key
                 ((name home db engine) identity)
                 ((data) (lambda (v) (read-config-blog-data (make-hash-table) v)))
                 ((interface) (lambda (v) (read-config-blog-interface (make-hash-table) v)))
                 ((appearance) (lambda (v) (read-config-blog-appearance (make-hash-table) v)))
                 (else (lambda (_) '%INVALID%))))
             (val (process-value val*)))
        (unless (eqv? val '%INVALID%)
          (hash-table-set! cfg key val))
        (loop (cdr params))))))

(define (read-config file)
  (let ((config (%config%)))
    #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USERS, SESSIONS, AUTHENTICATION  ---------------------------------

(define (register-user uname password email role #!optional (disp-name '()))
  (let ((phash (string->sha1sum password)))
    (unless (member role '("editor" "author" "member"))
      (error (string-append "'" role "' is not a recognized role.")))
    (call-with-database
      (make-pathname (make-pathname (%blog-root%) "data") "example.db")
      (lambda (conn)
        (db:current-connection conn)
        (db:add-user uname phash email role disp-name))))) 

(define (login uname password)
  (call-with-database
    (make-pathname (make-pathname (%blog-root%) "data") "example.db")
    (lambda (conn)
      (db:current-connection conn)
      (if (db:can-login? uname)
        (let ((phash (string->sha1sum password)))
          (if (string=? phash (db:get-passhash uname))
            ;;; FIXME: obviously bogus code here!
            #t
            #f))))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

(define (default-teaser article-body)
  (let ((alen (string-length article-body))
        (tlen (%default-teaser-length%)))
    (if (< tlen alen)
      (substring article-body 0 tlen)
      article-body)))

(define (get-template tpl-name #!optional (content-type 'text/html))
  (let* ((file-ext
           (case content-type
             ((text/html) "html")
             ((application/json) "json")
             (else (eprintf "[get-template] Unknown content type: ~A" content-type))))
         (tpl-path
          (make-pathname
            (make-pathname (%blog-root%) "templates")
            tpl-name file-ext)))
    (if (file-exists? tpl-path)
      tpl-path
      (eprintf "[get-template] Template file '~A' not found." tpl-path))))

; (define (teaser article-id)
;   (or (get-teaser article-id)
;       (default-teaser (get-body article-id))))
; 
; (define (latest #!optional (n 10))
;   (for-each
;     (lambda (id) (teaser id))
;     (index-ref 'post-order 10)))

(define (init blog-root)
  (%blog-root% blog-root)
  (call-with-database
    (make-pathname (make-pathname blog-root "data") "example.db")
    (lambda (conn)
      (db:current-connection conn)
      (for-each
        db:add-role
        '("admin" "editor" "author" "member" "guest")))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  WEB INTERFACE  -----------------------------------------------------

(define (get-article/html id/alias #!optional (out (current-output-port)))
  (let* ((article-data
          (if (node-id? id/alias)
            (db:get-article-by-nodeid id/alias)
            (db:get-article-by-alias id/alias)))
         (raw-body (alist-ref 'body (alist-ref 'content article-data)))
         (sanitized-body (escape-html raw-body))
         (html-body (markdown->html sanitized-body)))
    (display html-body out)))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
