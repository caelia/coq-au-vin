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

(define listen-port
  (let ((env-port (get-environment-variable "CAV_PORT")))
    (if env-port
      (string->number env-port)
      3125)))

(define (log-obj msg obj #!optional (logfile "obj.log"))
  (with-output-to-file
    logfile
    (lambda ()
      (print msg)
      (pp obj))))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (request-handler in out err env)
  (let* ((env* (env))
         (method (alist-ref "REQUEST_METHOD" env* string=?))
         (path-str (alist-ref "REQUEST_URI" env* string=?))
         (path (uri-path (uri-reference path-str)))
         (spec (list path method))
         (send-page
           (lambda (type data)
             (let ((len (string-length data)))
               (out (sprintf "Content-type: ~A\r\n" type))
               (out (sprintf "Content-length: ~A\r\n\r\n" len))
               (out data))))
         (send-html
           (lambda (data)
             (send-page "text/html" data)))
         (send-json
           (lambda (data)
             (send-page "application/json" data))))
    ; (logerr (with-output-to-string (lambda () (pretty-print env*))))
    (match spec
      [(or ((/ "") "GET") ((/ "articles") "GET"))
       (send-html (get-article-list-page/html out: #f))]
      [((/ "articles" id/alias) "GET")
       (send-html (get-article-page/html id/alias out: #f))]
      [((or (/ "series") (/ "series" "")) "GET")
       (send-html (get-meta-list-page/html 'series #f))]
      [((/ "series" series-title) "GET")
       (send-html (get-article-list-page/html criterion: `(series ,series-title) out: #f))]
      [((or (/ "tags") (/ "tags" "")) "GET")
       (send-html (get-meta-list-page/html 'tags #f))]
      [((/ "tags" tag) "GET")
       (send-html (get-article-list-page/html criterion: `(tag ,tag) out: #f))]
      [((or (/ "authors") (/ "authors" "")) "GET")
       (send-html (get-meta-list-page/html 'authors #f))]
      [((/ "authors" author) "GET")
       (send-html (get-article-list-page/html criterion: `(author ,author) out: #f))]
      [((or (/ "categories") (/ "categories" "")) "GET")
       (send-html (get-meta-list-page/html 'categories #f))]
      [((/ "categories" category) "GET")
       (send-html (get-article-list-page/html criterion: `(category ,category) out: #f))]
      [_
        (out "Status: 404 Not Found\r\n\r\n")])))

(define (run)
  ;(enable-sqlite)
  (fcgi-accept-loop listen-port 0 request-handler))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
