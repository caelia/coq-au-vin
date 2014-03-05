;;; cav-web-fcgi.scm -- FastCGI front end for Coq-au-Vin
;;;
;;;   Copyright © 2013-2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module cav-web-fcgi
        *
        (import scheme chicken)
        (import ports)
        (import data-structures)
        (import extras)
        (use coq-au-vin)
        (use cav-db-sqlite)
        (use fastcgi)
        (use uri-common)
        (use matchable)
        (use utf8-srfi-13)
        (use intarweb)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define %domain% (make-parameter #f))

;;; ========================================================================
;;; ------------------------------------------------------------------------

(define (log-obj msg obj #!optional (logfile "obj.log"))
  (with-output-to-file
    logfile
    (lambda ()
      (print msg)
      (pp obj))
    #:append))

(define (alist-stref key alist)
  (alist-ref key alist string=?))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (set-session-key hdrs key)
  (let ((cookie-header
          (sprintf "SessionKey=~A; Domain=~A; Secure; HttpOnly\r\n" key (%domain%))))
    (headers `((set-cookie . ,cookie-header)) hdrs)))

(define (set-strict-tls hdrs #!optional (max-age 63072000) (include-subdomains #t))
  (headers 
    '((strict-transport-security . ((max-age . ,max-age)
                                    (include-subdomains . ,include-subdomains))))
     hdrs))

(define (get-session-key env*)
  (let* ((cookie-string (alist-stref "HTTP_COOKIE" env*))
         (cookies
           (if cookie-string
             (map string-trim-both (string-split cookie-string ";"))
             '())))
    (let loop ((cookies* cookies))
      (if (null? cookies*)
        #f
        (let* ((cookie (car cookies*))
               (k+v (map string-trim-both (string-split cookie "=")))
               (key (car k+v))
               (val (cadr k+v)))
          (if (string=? "SessionKey" key)
            val
            (loop (cdr cookies*))))))))

;(define-syntax with-authorization
;  (syntax-rules ()
;    ((_ key action ip body0 body ...)
;     (if (authorized? key action ip)
;       (begin
;         body0
;         body
;         ...)
;       (unauthorized 
     
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (send-page out response body #!key (type 'html) (session-key #f) (https #f) (extra-headers '()))
  (let* ((typestr
           (case type
             ((html) "text/html")
             ((json) "application/json")))
         (hdrs*
           (headers
             `((content-type . ,typestr) (content-length . ,(string-length body)) ,@extra-headers)
             (response-headers response)))
         (hdrs
           (cond
             (session-key
               (set-strict-tls (set-session-key hdrs* session-key)))
             (https
               (set-strict-tls hdrs*))
             (else
               hdrs*)))
         (resp* (update-response response headers: hdrs)))
      (out
        (with-output-to-string
          (lambda ()
            (let ((resp (update-response resp* port: (current-output-port))))
              (write-response resp)
              (display body)
              (finish-response-body resp)))))))

(define (request-handler in out err env)
  (let* ((env* (env))
         (method (alist-stref "REQUEST_METHOD" env*))
         (path-str* (alist-stref "REQUEST_URI" env*))
         (qstring (alist-stref "QUERY_STRING" env*))
         (https? (equal? (alist-stref "HTTPS" env*) "on"))
         (path-str (if (string=? path-str* "/") "/" (string-trim-right path-str* #\/)))
         (path (uri-path (uri-reference path-str)))
         (query (form-urldecode qstring))
         (offset (alist-ref 'offset query))
         (spec (list path method offset))
         (send-html
           (lambda (data #!optional (require-https #f) (extra-headers '()))
             (send-page out (make-response) data https: (or require-https https?) extra-headers: extra-headers)))
         (send-json
           (lambda (data #!optional (require-https #f) (extra-headers '()))
             (send-page out (make-response) data type: 'json https: (or require-https https?) extra-headers: extra-headers)))
         (when-authorized
           (lambda (action-key action #!optional (type 'html))
             (let ((client-ip (alist-stref "REMOTE_ADDR" env*))
                   (referer (or (alist-stref "HTTP_REFERER" env*) "/"))
                   (session-key (get-session-key env*)))
               (cond
                 ((and https? (authorized? session-key action-key client-ip)) (action))
                 ((eqv? type 'html) (send-html (unauthorized-message/html referer #f)))
                 ((eqv? type 'json) (send-json (unauthorized-message/json referer #f))))))))
    (handle-exceptions
      exn
      (err (with-output-to-string (lambda () (pp exn))))
      (match spec
        [(or ((/ "") "GET" #f) ((/ "articles") "GET" #f))
         (send-html (get-article-list-page/html out: #f))]
        [(or ((/ "") "GET" ofs) ((/ "articles") "GET" ofs))
         (send-html (get-article-list-page/html out: #f offset: (string->number ofs)))]
        [((/ "articles" "new") "GET" _)
         (when-authorized 'create-article
                          (lambda () (send-html (get-new-article-form/html #f))))]
        [((/ "articles" "new") "POST" _)
         (let* ((raw-form (fcgi-get-post-data in env))
                (form-data (form-urldecode raw-form)))
           (when-authorized 'create-article
                            (lambda () (send-html (add-article form-data #f)))))]
        [((/ "articles" id/alias) "POST" _)
         (let* ((raw-form (fcgi-get-post-data in env))
                (form-data (form-urldecode raw-form)))
           (when-authorized 'edit-article
                            (lambda () (send-html (update-article id/alias form-data #f)))))]
        [((/ "articles" id/alias "edit") "GET" _)
         (when-authorized 'edit-article
                          (lambda () (send-html (get-article-edit-form/html id/alias #f))))]
        [((/ "articles" id/alias) "GET" _)
         (send-html (get-article-page/html id/alias out: #f))]
        [((or (/ "series") (/ "series" "")) "GET" _)
         (send-html (get-meta-list-page/html 'series #f))]
        [((/ "series" series-title) "GET" #f)
         (send-html (get-article-list-page/html criterion: `(series ,series-title) out: #f))]
        [((/ "series" series-title) "GET" ofs)
         (send-html (get-article-list-page/html criterion: `(series ,series-title) out: #f offset: (string->number ofs)))]
        [((or (/ "tags") (/ "tags" "")) "GET" _)
         (send-html (get-meta-list-page/html 'tags #f))]
        [((/ "tags" tag) "GET" #f)
         (send-html (get-article-list-page/html criterion: `(tag ,tag) out: #f))]
        [((/ "tags" tag) "GET" ofs)
         (send-html (get-article-list-page/html criterion: `(tag ,tag) out: #f offset: (string->number ofs)))]
        [((or (/ "authors") (/ "authors" "")) "GET" _)
         (send-html (get-meta-list-page/html 'authors #f))]
        [((/ "authors" author) "GET" #f)
         (send-html (get-article-list-page/html criterion: `(author ,author) out: #f))]
        [((/ "authors" author) "GET" ofs)
         (send-html (get-article-list-page/html criterion: `(author ,author) out: #f offset: (string->number ofs)))]
        [((or (/ "categories") (/ "categories" "")) "GET" _)
         (send-html (get-meta-list-page/html 'categories #f))]
        [((/ "categories" category) "GET" #f)
         (send-html (get-article-list-page/html criterion: `(category ,category) out: #f))]
        [((/ "categories" category) "GET" ofs)
         (send-html (get-article-list-page/html criterion: `(category ,category) out: #f offset: (string->number ofs)))]
        [((/ "login") "GET" _)
         (send-html (get-login-form/html #f) #t)]
        [((/ "login") "POST" _)
         (let* ((raw-form (fcgi-get-post-data in env))
                (form-data (form-urldecode raw-form))
                (client-ip (alist-stref "REMOTE_ADDR" env*)))
           (let-values (((page session) (webform-login form-data client-ip #f)))
             (cond
               ((and session https?)
                (send-page out (make-response) page session-key: session https: #t))
               (https? (send-html page))
               (else (out (with-output-to-string (lambda () (write-response (make-response status: 'forbidden)))))))))]
        [_
          (out
            (with-output-to-string
              (lambda ()
                (write-response (make-response status: 'not-found)))))]))))

(define (run listen-port)
  (fcgi-accept-loop listen-port 0 request-handler))


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
