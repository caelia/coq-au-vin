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

        (use (prefix cav-db db:))
        (use (prefix civet cvt:))
  
        (use lowdown)
        (use srfi-69)
        (use random-bsd)
        (use crypt)

        (use utf8)
        (use utf8-srfi-13)
        (use utf8-srfi-14)

        (use uri-common)
        ;; KLUDGING s11n!
        (use sxml-transforms)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS & CONSTANTS ------------------------------------

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

;;; This should only be set to #t for demo/debugging purposes!
(define %waive-authorization% (make-parameter #f))

(define +lucky-number+ (random-fixnum 1000000))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (dump-sessions)
  (hash-table-for-each
    session-store
    (lambda (k v) (printf "~A: ~A\n" k v))))

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

(define (text->teaser str #!optional (n (%default-teaser-length%)))
  (if (< n 1)
    ""
    (let ((return-result
            (lambda (idx)
              (string-append (substring str 0 idx) " ... \n"))))
      (with-input-from-string str
        (lambda ()
          (let loop ((state 'init)
                     (chr (read-char))
                     (index 0)
                     (word-count 0))
            (cond
              ((eof-object? chr)
               (return-result index))
              ((and (eqv? state 'word)
                    (char-set-contains? char-set:whitespace chr))
               (let ((new-count (+ word-count 1)))
                 (if (>= new-count n)
                   (return-result index)
                   (loop 'space (read-char) (+ index 1) new-count))))
              ((char-set-contains? char-set:whitespace chr)
               (loop 'space (read-char) (+ index 1) word-count))
              (else
                (loop 'word (read-char) (+ index 1) word-count)))))))))

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

;;; The big bad s11n kludge!
(define sxml-normalization-rules
  `((*text* . ,(lambda (_ x) (->string x)))
    (*default* . ,(lambda (tag children)
                    (cons tag
                          (append-map
                           (lambda (x)
                             (cond ((not (list? x)) (list x))
                                   ((null? x) x)
                                   ((symbol? (car x)) (list x))
                                   (else x)))
                           children))))))

(define (normalize-sxml doc)
  (pre-post-order* doc sxml-normalization-rules))

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

(define (mk-session-key uname)
  (crypt
    (sprintf "~A:~A:~A:~A" uname (current-seconds) +lucky-number+ (random 4096))))

(define session-store (make-hash-table))

(define-record-type session-data
  (make-session-data uname ip role expires)
  session-data?
  (uname session-uname)
  (ip session-ip)
  (role session-role)
  (expires session-expires session-expires-set!))

(define-record-type credentials
  (make-credentials session ip) 
  credentials?
  (session cred-session)
  (ip cred-ip))

(define (create-session! key uname ip role expires)
  (let* ((sdata (make-session-data uname ip role expires)))
    (hash-table-set! session-store key sdata)))

(define (delete-session! key)
  (hash-table-delete! session-store key)
  #f)

(define (session-expired? key session)
  (let ((expired (< (session-expires session) (current-seconds))))
    (if expired
      (not (delete-session! key))
      #f)))

(define (session-exists? key)
  (hash-table-ref/default session-store key #f))

(define (session-valid? key session ip)
  (and key
       session
       (not (session-expired? key session))
       (string=? (session-ip session) ip)))

(define (session-refresh! session new-exp)
  (session-expires-set! session new-exp))

;; FIXME: need to check that user is logged in for correct IP
(define (logged-in? uname)
  (let* ((store-contents (hash-table->alist session-store))
         (sess
           (find
             (lambda (elt)
               (string=? (session-uname (cdr elt)) uname))
             store-contents)))
    (and sess
         (let ((key (car sess))
               (obj (cdr sess)))
           (if (session-expired? key obj)
             (delete-session! key)
             key)))))

(define (authorized? session-key action ip #!optional (resource #f))
  (or (eqv? action 'view-content)
      (and session-key
           (let ((session
                   (and (hash-table-exists? session-store session-key)
                        (hash-table-ref session-store session-key))))
             (and (session-valid? session-key session ip)
                  (let ((role (string->symbol (session-role session))))
                    (cond
                      ((eqv? role 'admin) #t)
                      ((eqv? action 'create-article)
                       (or (eqv? role 'editor) (eqv? role 'author)))
                      ((eqv? action 'edit-article)
                       (eqv? role 'editor))
                      (else
                        #f))))))))

;(define (authorize credentials action #!optional (resource #f))
  ;(or (%waive-authorization%)
      ;(authorized? session-key action ip resource)
      ;(abort (make-property-condition 'unauthorized))))

(define (register-roles #!optional (roles (%default-roles%)))
  ;((db:connect))
  (for-each (db:add-role) roles)
  ;((db:disconnect)))
  )

(define (register-user uname password email role #!optional (disp-name '()))
  (let ((blank? (lambda (s) (and (string? s) (string=? (string-trim-both s) "")))))
    (when (or (blank? uname) (blank? password)
              (blank? email) (blank? role))
      (error "One or more required fields is blank."))
    ((db:connect))
    (let ((roles (flatten ((db:get-roles)))))
      (unless (member role roles)
      ((db:disconnect))
      (eprintf "'~A' is not a recognized role.\n" role)))
    (let ((phash (crypt password)))
      (if (blank? disp-name)
        ((db:add-user) uname phash email role))
        ((db:add-user) uname phash email role disp-name))
    ((db:disconnect))))

(define (login uname password ip #!optional (keygen mk-session-key))
  ((db:connect))
  (let ((result
          (and ((db:can-login?) uname)
               (let ((stored-hash ((db:get-passhash) uname)))
                 (if (and stored-hash
                          (string=? stored-hash (crypt password stored-hash)))
                   (let* ((logged-in (logged-in? uname))
                          (session-key (or logged-in (keygen uname)))
                          (exp-time (+ (current-seconds) (%session-timeout%))))
                     (if logged-in
                       (session-refresh! session-key exp-time)
                       (let ((role ((db:get-user-role) uname)))
                         (create-session! session-key uname ip role exp-time)))
                     session-key)
                   (begin
                     ((db:bad-login) uname)
                     #f))))))
    ((db:disconnect))
    result))

(define (logout #!key (uname #f) (session #f))
  (when (or uname session)
    (let ((session-key (or session (logged-in? uname))))
      (when session-key
        (delete-session! session-key)))))

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
  
(define (process-body article-data #!optional (raw #f))
  (let* ((content (alist-ref 'content article-data))
         (raw-body (or (alist-ref 'body content) (alist-ref 'teaser content))))
    (if raw
      raw-body
      (let ((sanitized-body (escape-html raw-body)))
        (normalize-sxml (markdown->sxml sanitized-body))))))

(define (prepare-article-vars article-data date-format #!optional (raw #f))
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
           (cons (cons 'text (process-body article-data raw)) prev))
          ((categories tags)
           (cons pair prev))
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
                     ((authors tags categories) (split-list val))
                     (else (string-trim-both val))))))
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
                               (date-format #f) (logged-in #f))
  ((db:connect))
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data))
         (vars* (prepare-article-vars article-data date-format))
         (page-vars
           (config-get
             'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
             'copyright_holders 'rights_statement 'html_title 'body_classes))
         (vars `((article_id . ,id/alias) (logged_in . ,logged-in) ,@page-vars ,@vars*))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "article.html" ctx port: out)))

(define (get-article-list-page/html #!key (out (current-output-port))
                                    (criterion 'all) (sort '(created desc))
                                    (date-format #f) (limit 10) (offset 0)
                                    (show 'teaser) (logged-in #f))
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
                 'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
                 'copyright_holders 'rights_statement 'html_title 'body_classes))
             (vars
               `((self_base_url . ,self-base-url) (simple_pager ,@simple-pager)
                 (articles ,@list-vars) (logged_in . ,logged-in) ,@page-vars))
             (ctx (cvt:make-context vars: vars)))
        ((db:disconnect))
        (cvt:render "article-list.html" ctx port: out)))))

(define (get-articles-by-date/html date #!key (out (current-output-port))
                                   (sort '(created desc)) (limit 10)
                                   (offset 0) (show 'teaser) (logged-in #f))
  #f)

(define (get-meta-list-page/html subject #!key (out (current-output-port)) (logged-in #f))
  ((db:connect))
  (let* ((list-data ((db:get-meta-list) subject))
         (page-vars
           (config-get
             'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
             'copyright_holders 'rights_statement 'html_title 'body_classes))
         (vars `((subject . ,(symbol->string subject)) (metadata_items . ,list-data)
                 (logged_in . ,logged-in) ,@page-vars))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "meta-list.html" ctx port: out)))

(define (get-article-list/json #!optional (out (current-output-port))
                               #!key (filters 'latest) (sort '(created desc))
                               (per-page 10) (show 'teaser) (logged-in #f))
  #f)

(define (get-new-article-form/html #!optional (out (current-output-port)))
  (let* ((page-vars
           (config-get
             'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
             'copyright_holders 'rights_statement 'html_title 'body_classes))
         (vars `((logged_in . #t) ,@page-vars))
         (ctx (cvt:make-context vars: vars)))
    (cvt:render "edit.html" ctx port: out)))

(define (get-article-edit-form/html id/alias #!optional (out (current-output-port)))
  ((db:connect))
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data))
         (vars* (prepare-article-vars article-data "%Y-%m-%d" #t))
         (page-vars
           (config-get
             'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
             'copyright_holders 'rights_statement 'html_title 'body_classes))
         (vars `((article_id . ,id/alias) (logged_in . #t) ,@page-vars ,@vars*))
         (ctx (cvt:make-context vars: vars)))
    ((db:disconnect))
    (cvt:render "edit.html" ctx port: out)))

(define (add-article form-data #!optional (out (current-output-port)))
  (let ((page-vars
          (config-get
            'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
            'copyright_holders 'rights_statement 'html_title 'body_classes)))
    ((db:connect))
    (let ((vars
            (let* ((data (prepare-form-data form-data))
                   (node-id (get-node-id))
                   (data+ (cons node-id (map cdr data))))
              (apply (db:create-article) data+)
              `((message . "Article successfully posted.") (msg_class . "info")
                (proceed_to . "/articles") (logged_in . #t) ,@page-vars))))
      ((db:disconnect))
      (cvt:render "msg.html" (cvt:make-context vars: vars) port: out))))

(define (update-article id/alias form-data #!optional (out (current-output-port)))
  (let ((page-vars
          (config-get
            'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
            'copyright_holders 'rights_statement 'html_title 'body_classes)))
    ((db:connect))
    (let ((vars
            (let* ((node-id
                     (if (node-id? id/alias)
                       id/alias
                       ((db:alias->node-id) id/alias)))
                   (data (prepare-form-data form-data #t))
                   (data+ (cons node-id (map cdr data))))
              (apply (db:update-article) data+)
              `((message . "Article successfully updated.") (msg_class . "info")
                (proceed_to . ,(string-append "/articles/" id/alias)) ,@page-vars))))
      ((db:disconnect))
      (cvt:render "msg.html" (cvt:make-context vars: vars) port: out))))

(define (get-login-form/html #!optional (out (current-output-port))) 
  (let ((vars
          (config-get
            'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
            'copyright_holders 'rights_statement 'html_title 'body_classes)))
    (cvt:render "login.html" (cvt:make-context vars: vars) port: out)))

(define (webform-login form-data ip #!optional (out (current-output-port)))
  (let* ((page-vars
           (config-get
             'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
             'copyright_holders 'rights_statement 'html_title 'body_classes))
         (uname (alist-ref 'uname form-data))
         (password (alist-ref 'password form-data))
         (session (login uname password ip))
         (vars
           (if session
             `((message . ,(sprintf "You are logged in as ~A." uname))
               (msg_class . "info") (proceed_to . "/") ,@page-vars)
             `((message . "Login failed.")
               (msg_class . "error") (proceed_to . "/login") ,@page-vars))))
    (values
      (cvt:render "msg.html" (cvt:make-context vars: vars) port: out)
      session)))

(define (unauthorized-message/html referer #!optional (out (current-output-port)))
  (let* ((page-vars
          (config-get
            'url_scheme 'host_name 'body_md 'jquery_src 'can_edit 'copyright_year
            'copyright_holders 'rights_statement 'html_title 'body_classes))
         (vars
           `((message . "You are not authorized to perform this action.")
             (msg_class . "error") (logged_in . #t) (proceed_to . ,referer)
             ,@page-vars)))
    (cvt:render "msg.html" (cvt:make-context vars: vars) port: out)))

(define (unauthorized-message/json referer #!optional (out (current-output-port)))
  #f)

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
