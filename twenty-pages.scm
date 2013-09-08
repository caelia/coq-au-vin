; (use coq-au-vin)
; (use (prefix cav-db db:))
; (load "cav-db-sqlite.scm")
; (import cav-db-sqlite)
; (use (prefix civet cvt:))
; (use sxml-transforms)
; (use sxml-serializer)

(include "demo-lib.scm")

(define output-path "examples/demo-site1/pages")

(let* ((ctx (get-article-list-ctx limit: 20))
       (sx (get-article-list-sxml ctx))
       (page-path (make-pathname output-path "index" "html")))
  (write-html sx page-path))

(for-each
  (lambda (id)
    (let* ((ctx (get-article-ctx id))
           (sx (get-article-sxml ctx))
           (page-path (make-pathname output-path id "html")))
      (write-html sx page-path)))
  (%current-articles%))
