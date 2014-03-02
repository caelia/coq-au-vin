;;; cav-web-fcgi.scm -- FastCGI front end for Coq-au-Vin
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(use coq-au-vin)
(use cav-db-sqlite)
(use fastcgi)
(use uri-common)
(use matchable)
(use utf8-srfi-13)

(define (log-obj msg obj #!optional (logfile "obj.log"))
  (with-output-to-file
    logfile
    (lambda ()
      (print msg)
      (pp obj))
    #:append))

(define (alist-stref key alist)
  (alist-ref key alist string=?))

(define (put-session-key out key)
  (out (sprintf "Set-Cookie: SessionKey=~A\r\n" key)))

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
     
;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (request-handler in out err env)
  (let* ((env* (env))
         (method (alist-stref "REQUEST_METHOD" env*))
         (path-str* (alist-stref "REQUEST_URI" env*))
         (qstring (alist-stref "QUERY_STRING" env*))
         (path-str (if (string=? path-str* "/") "/" (string-trim-right path-str* #\/)))
         (path (uri-path (uri-reference path-str)))
         (query (form-urldecode qstring))
         (offset (alist-ref 'offset query))
         (spec (list path method offset))
         (send-page
           (lambda (type data . headers)
             (let ((len (string-length data)))
               (out (sprintf "Content-Type: ~A\r\n" type))
               (out (sprintf "Content-Length: ~A\r\n" len))
               (for-each
                 (lambda (hdr)
                   (out (sprintf "~A: ~A\r\n" (car hdr) (cdr hdr))))
                 headers)
               (out "\r\n")
               (out data))))
         (send-html
           (lambda (data . extra-headers)
             (apply send-page `("text/html" ,data ,@extra-headers))))
         (send-json
           (lambda (data . extra-headers)
             (apply send-page `("application/json" ,data ,@extra-headers))))
         (when-authorized
           (lambda (action-key action #!optional (type 'html))
             (let ((client-ip (alist-stref "REMOTE_ADDR" env*))
                   (referer (or (alist-stref "HTTP_REFERER" env*) "/"))
                   (session-key (get-session-key env*)))
               (cond
                 ((authorized? session-key action-key client-ip) (action))
                 ((eqv? type 'html) (send-html (unauthorized-message/html referer #f)))
                 ((eqv? type 'json) (send-json (unauthorized-message/json referer #f))))))))
    (match spec
      [(or ((/ "") "GET" #f) ((/ "articles") "GET" #f))
       (send-html (get-article-list-page/html out: #f))]
      [(or ((/ "") "GET" #f) ((/ "articles") "GET" ofs))
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
       (send-html (get-login-form/html #f))]
      [((/ "login") "POST" _)
       (let* ((raw-form (fcgi-get-post-data in env))
              (form-data (form-urldecode raw-form))
              (client-ip (alist-stref "REMOTE_ADDR" env*)))
         (let-values (((page session) (webform-login form-data client-ip #f)))
           (if session
             (send-html page `("Set-Cookie" . ,(sprintf "SessionKey=~A" session)))
             (send-html page))))]
      [_
        (out "Status: 404 Not Found\r\n\r\n")])))

(define (run listen-port)
  (fcgi-accept-loop listen-port 0 request-handler))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
