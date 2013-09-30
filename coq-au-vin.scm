;;; coq-au-vin.scm -- A simple blogging platform based on Chicken Scheme.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module coq-au-vin
        *
        (import scheme chicken)
        (import files)
        (import ports)
        (import data-structures)
        (import extras)
        (import posix)
        (import srfi-1)
        ; (import srfi-13)

        (use (prefix cav-db db:))
        (use (prefix civet cvt:))
  
        (use lowdown)
        (use srfi-69)
        ; (use crypt)
        ; ;; FIXME: Need a better password hash! 
        (use simple-sha1)

        ; (use spiffy)
        ; (use intarweb)
        ; (use uri-match)

        (use utf8)
        (use utf8-srfi-13)
        (use utf8-srfi-14)

        (use uri-common)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS  -----------------------------------------------

(define %blog-root% (make-parameter #f))

; (define %default-teaser-length% (make-parameter 1024))
; Should be defined in words
(define %default-teaser-length% (make-parameter 64))

(define %config% (make-parameter (make-hash-table)))

(define %session-timeout% (make-parameter 900))

(define %default-date-format% (make-parameter #f))

(define %object-log-file% (make-parameter "obj.log"))

(define %first-node-id% (make-parameter "10000001"))

(define %default-roles% (make-parameter '("admin" "editor" "author" "member" "guest")))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (log-obj msg obj #!optional (logfile (%object-log-file%)))
  (with-output-to-file
    logfile
    (lambda ()
      (print msg)
      (pp obj))
    #:append))
 
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

;;; Uh-oh: this ceases to work when there are more than 805,306,366 nodes!
(define (next-node-id nid)
  (let* ((hexstr (string-append "#x" nid))
         (numeric (string->number hexstr))
         (next (+ numeric 1)))
    (sprintf "~X" next)))

(define (get-node-id) 
  (let ((last-nid ((db:get-last-id))))
    (if (null? last-nid)
      (%first-node-id%)
      (next-node-id (car last-nid)))))

; This is not absolutely foolproof, but I think it's close enough
(define (node-id? s)
  (and (= (string-length s) 8)
       (string->number (string-append "#x" s))))

(define (words str n)
  (if (< n 1)
    '()
    (with-input-from-string str
      (lambda ()
        (let ((return-result
                (lambda (current-word words-out)
                  (reverse
                    (if (null? current-word)
                      words-out
                      (cons
                        (list->string (reverse current-word))
                        words-out))))))
          (let loop ((state 'init) (count 1) (chr (read-char)) (current-word '()) (words-out '()))
            (cond
              ((eof-object? chr)
               (return-result current-word words-out))
              ((char-set-contains? char-set:whitespace chr)
               (case state
                 ((init)
                  (loop 'init count (read-char) '() '()))
                 ((word)
                  (if (>= count n)
                    (return-result current-word words-out)
                    (loop 'space count (read-char) '() (cons (list->string (reverse current-word)) words-out))))
                 ((space)
                  (loop 'space count (read-char) '() words-out))))
              (else
                (case state
                  ((init)
                   (loop 'word count (read-char) (cons chr current-word) '()))
                  ((word)
                   (loop 'word count (read-char) (cons chr current-word) words-out))
                  ((space)
                   (loop 'word (+ count 1) (read-char) (list chr) words-out)))))))))))

(define (text->teaser txt #!optional (length #f))
  (let ((wds (words txt (or length (%default-teaser-length%)))))
    (string-append (string-join wds " ") " ...")))

(define (title->alias title)
  (with-output-to-string
    (lambda ()
      (with-input-from-string title
        (lambda ()
          (let loop ((chr (read-char)))
            (cond
              ((eof-object? chr) #t)
              ((char-set-contains? char-set:whitespace chr)
               (write-char #\-)
               (loop (read-char)))
              ((char-set-contains? char-set:punctuation chr)
               (loop (read-char)))
              (else
                (write-char (char-downcase chr))
                (loop (read-char))))))))))

(define (config-set! . vars)
  (let ((cfg-data (%config%)))
    (for-each
      (lambda (elt)
        (let ((k (car elt)) (v (cdr elt))) (hash-table-set! cfg-data k v)))
      vars)))

(define (config-get . vars)
  (let ((cfg-data (%config%)))
    (foldl
      (lambda (prev key)
        (if (hash-table-exists? cfg-data key)
          `((,key . ,(hash-table-ref cfg-data key)) ,@prev)
          prev))
      '()
      vars)))

(define (config key . vals)
  (let ((cfg-data (%config%)))
    (if (null? vals)
      (hash-table-ref cfg-data key)
      (hash-table-set! cfg-data key (car vals)))))

(define (config*)
  (hash-table->alist (%config%)))

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

(define (register-roles #!optional (roles (%default-roles%)))
  ;((db:connect))
  (for-each (db:add-role) roles)
  ;((db:disconnect)))
  )

(define (register-user uname password email role #!optional (disp-name '()))
  (let ((blank? (lambda (s) (string=? (string-trim-both s) ""))))
    (when (or (blank? uname) (blank? password)
              (blank? email) (blank? role))
      (error "One or more required fields is blank.")))
  ((db:connect))
  (let ((roles (flatten ((db:get-roles)))))
    (unless (member role roles)
      ((db:disconnect))
      (eprintf "'~A' is not a recognized role.\n" role)))
  (let ((phash (string->sha1sum password)))
    ((db:add-user) uname phash email role disp-name))
  ((db:disconnect)))

; (define (login uname password)
;   (call-with-database
;     (make-pathname (make-pathname (%blog-root%) "data") "example.db")
;     (lambda (conn)
;       ((db:current-connection) conn)
;       (if ((db:can-login?) uname)
;         (let ((phash (string->sha1sum password)))
;           (if (string=? phash ((db:get-passhash) uname))
;             ;;; FIXME: obviously bogus code here!
;             #t
;             #f))))))

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

; These procedures are probably needed, but currently they use the sql-de-lite
; API directly, so the DB functionality needs to be abstracted.

; (define (teaser article-id)
;   (or (get-teaser article-id)
;       (default-teaser (get-body article-id))))
; 
; (define (latest #!optional (n 10))
;   (for-each
;     (lambda (id) (teaser id))
;     (index-ref 'post-order 10)))

; (define (init blog-root)
;   (%blog-root% blog-root)
;   (call-with-database
;     (make-pathname (make-pathname blog-root "data") "example.db")
;     (lambda (conn)
;       ((db:current-connection) conn)
;       (for-each
;         db:add-role
;         '("admin" "editor" "author" "member" "guest")))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  WEB INTERFACE  -----------------------------------------------------

;;; ------  Lo-level  ------------------------------------------------------

(define (get-article-data id/alias)
  (if (node-id? id/alias)
    ((db:get-article-by-nodeid) id/alias)
    ((db:get-article-by-alias) id/alias)))

(define (get-article-list-data #!key (tag #f) (author #f) (series #f)
                               (limit 10) (offset 0) (teaser-len #f))
  #f)
  
(define (process-body article-data)
  (let* ((content (alist-ref 'content article-data))
         (raw-body (or (alist-ref 'body content) (alist-ref 'teaser content)))
         (sanitized-body (escape-html raw-body)))
;     (with-output-to-string
;       (lambda () (markdown->html sanitized-body)))))
    (markdown->sxml sanitized-body)))

(define (prepare-article-vars article-data date-format)
  (foldl
    (lambda (prev pair)
      (let ((key (car pair))
            (val (cdr pair)))
        (case key
          ((authors)
           (cons (cons 'authors val) prev))
          ((created_dt)
           (let* ((fmt (or date-format (%default-date-format%)))
                  (dtstring (time->string (seconds->local-time val) fmt)))
             (cons
               (cons 'created_dt dtstring)
               (cons
                 (cons 'raw_dt val)
                 prev))))
          ((title)
           (cons (cons 'article_title val) prev))
          ((content)
           (cons (cons 'text (process-body article-data)) prev))
          (else
            (let ((res (if (null? val) (cons key "") pair)))
              (cons res prev))))))
    '()
    article-data))

(define (make-simple-pager count per-page offset)
  (and (> count per-page)
       (cond
         ((= offset 0) `((newer . #f) (older . ,(number->string per-page))))
         ((>= (+ offset per-page) count) `((newer . ,(number->string (- offset per-page))) (older . #f)))
         (else `((newer . ,(number->string (- offset per-page))) (older . ,(number->string (+ offset per-page))))))))

(define (split-list list-string)
  (map
    string-trim-both
    (string-split list-string ",")))

(define (prepare-form-data form-data #!optional (update #f))
  (let* ((verify-field
          (lambda (fname)
            (let ((val (alist-ref fname form-data)))
              (and val
                   (> (string-length (string-trim-both val)) 0)))))
         (required-present
           (or update
               (and (verify-field 'title)
                    (verify-field 'body)
                    (verify-field 'authors)))))
    (and required-present
         (let ((prepare-value
                 (lambda (key val)
                   (case key
                     ((authors tags series) (split-list val))
                     (else val)))))
           (let loop ((keys '(title series subtitle alias sticky authors categories tags body))
                      (output '()))
             (if (null? keys)
               (reverse output)
               (let* ((key (car keys))
                      (val* (alist-ref key form-data))
                      (val
                        (cond
                          ((null? val*) '())
                          (val* (prepare-value key val*))
                          (else '()))))
                 (loop (cdr keys) `((,key . ,val) . ,output)))))))))

; (define (make-full-pager count per-page offset)
;   (and (> count per-page)
;        (let ((npages (inexact->exact (ceiling (/ count per-page))))
;              (this-page (/ offset per-page)))
;          (cond
;            ((< npages 8)

;;; ========================================================================
;;; ------  Hi-level  ------------------------------------------------------

(define (get-article/html id/alias #!optional (out (current-output-port)))
  (let* ((article-data (get-article-data id/alias))
         (html-body (process-body article-data)))
    (display html-body out)))

(define (get-article/json id/alias #!optional (out (current-output-port)))
  #f)

(define (get-article-page/html id/alias #!key (out (current-output-port))
                               (date-format #f))
  ((db:connect))
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data))
         (vars* (prepare-article-vars article-data date-format))
         (page-vars
           (config-get
             'urlScheme 'hostName 'bodyMD 'jquerySrc 'canEdit 'copyright_year
             'copyright_holders 'rights_statement 'htmlTitle 'bodyClasses))
         (vars `((articleID . ,id/alias) ,@page-vars ,@vars*))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "article.html" ctx port: out)))

(define (get-article-list-page/html #!key (out (current-output-port))
                                    (criterion 'all) (sort '(created desc))
                                    (date-format #f) (limit 10) (offset 0)
                                    (show 'teaser))
  ((db:connect))
  (let ((mkteaser
          (case show
            ((teaser) text->teaser)
            ((all) identity)
            (else (lambda (_) "")))))
    (let-values (((count list-data)
                  ((db:get-article-list) criterion limit offset mkteaser)))
      (let* ((self-base-url
               (string-append
                 "/"
                 (if (eqv? criterion 'all)
                   "articles"
                   (string-append
                     (case (car criterion)
                       ((tag) "tags")
                       ((series) "series")
                       ((author) "authors")
                       ((category) "categories"))
                     "/"
                     (cadr criterion)))))
             (simple-pager
               (make-simple-pager count limit offset))
             ;(full-pager
             ;  (make-full-pager count limit offset))
             (list-vars
               (map
                 (lambda (datum) (prepare-article-vars datum date-format))
                 list-data))
             (page-vars
               (config-get
                 'urlScheme 'hostName 'bodyMD 'jquerySrc 'canEdit 'copyright_year
                 'copyright_holders 'rights_statement 'htmlTitle 'bodyClasses))
             (vars
               `((self_base_url . ,self-base-url) (simple_pager ,@simple-pager)
                 (articles ,@list-vars) ,@page-vars))
             (ctx (cvt:make-context vars: vars)))
        ((db:disconnect))
        (cvt:render "article-list.html" ctx port: out)))))

(define (get-articles-by-date/html date #!key (out (current-output-port))
                                   (sort '(created desc)) (limit 10)
                                   (offset 0) (show 'teaser))
  #f)

(define (get-meta-list-page/html subject #!optional (out (current-output-port)))
  ((db:connect))
  (let* ((list-data ((db:get-meta-list) subject))
         (page-vars
           (config-get
             'urlScheme 'hostName 'bodyMD 'jquerySrc 'canEdit 'copyright_year
             'copyright_holders 'rights_statement 'htmlTitle 'bodyClasses))
         (vars `((subject . ,(symbol->string subject)) (metadata_items . ,list-data) ,@page-vars))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "meta-list.html" ctx port: out)))

(define (get-article-list/json #!optional (out (current-output-port))
                               #!key (filters 'latest) (sort '(created desc))
                               (per-page 10) (show 'teaser))
  #f)

(define (get-new-article-form/html #!optional (out (current-output-port)))
  (let* ((vars
           (config-get
             'urlScheme 'hostName 'bodyMD 'jquerySrc 'canEdit 'copyright_year
             'copyright_holders 'rights_statement 'htmlTitle 'bodyClasses))
         (ctx (cvt:make-context vars: vars)))
    (cvt:render "edit.html" ctx port: out)))

(define (get-article-edit-form/html id/alias #!optional (out (current-output-port)))
  ((db:connect))
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data))
         (vars* (prepare-article-vars article-data "%Y-%m-%d"))
         (page-vars
           (config-get
             'urlScheme 'hostName 'bodyMD 'jquerySrc 'canEdit 'copyright_year
             'copyright_holders 'rights_statement 'htmlTitle 'bodyClasses))
         (vars `((articleID . ,id/alias) ,@page-vars ,@vars*))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "edit.html" ctx port: out)))

(define (add-article form-data #!optional (out (current-output-port)))
  ((db:connect))
  (let* ((data (prepare-form-data form-data))
         (node-id (get-node-id))
         (data+ (cons node-id (map cdr data))))
    (apply (db:create-article) data+)
    ((db:disconnect))
    (let ((ctx (cvt:make-context vars: '((message . "Article successfully posted.") (msg_class . "info")))))
      (cvt:render "message.html" ctx port: out))))

(define (update-article id/alias form-data #!optional (out (current-output-port)))
  ((db:connect))
  (let* ((node-id (if (node-id? id/alias) id/alias ((db:alias->node-id) id/alias)))
         (data (prepare-form-data form-data #t))
         (data+ (cons node-id (map cdr data))))
    (apply (db:update-article) data+)
    ((db:disconnect))
    (let ((ctx (cvt:make-context vars: '((message . "Article successfully updated.") (msg_class . "info")))))
      (cvt:render "message.html" ctx port: out))))

(define (app-init #!key (site-path #f) (template-path #f))
  (when site-path
    (cvt:*site-path* site-path))
  (when template-path
    (cvt:*template-path* template-path)))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
