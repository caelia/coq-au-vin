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

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define (request-handler in out err env)
  (let* ((env* (env))
         (method (alist-ref "REQUEST_METHOD" env* string=?))
         (path-str (alist-ref "REQUEST_URI" env* string=?))
         (path (uri-reference path-str))
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
       (send-html (get-article-list/html))]
      [((/ "articles" id/alias) "GET")
       (send-html (get-article-page/html id/alias))]
      [((/ "series") "GET")
       (send-html (get-series-list/html))]
      [((/ "series" series-title) "GET")
       (send-html (get-series-articles/html series-title))]
      [((/ "tags") "GET")
       (send-html (get-tag-list/html))]
      [((/ "tags" tag) "GET")
       (send-html (get-articles-with-tag/html tag))]
      [((/ "authors") "GET")
       (send-html (get-author-list/html))]
      [((/ "authors" author) "GET")
       (send-html (get-articles-by-author/html author))]
      [((/ "categories") "GET")
       (send-html (get-category-list/html))]
      [((/ "category" category) "GET")
       (send-html (get-articles-by-category/html category))]
      [_
        (out "Status: 404 Not Found\r\n\r\n")])))

(define (run)
  (activate-sqlite)
  (fcgi-accept-loop listen-port 0 request-handler))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
