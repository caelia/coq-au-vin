; (load "coq-au-vin.scm")
; (import coq-au-vin)
; (use (prefix cav-db db:))
; (load "cav-db-sqlite.scm")
; (import cav-db-sqlite)
; (use (prefix civet cvt:))
; (use sxml-transforms)
; (use sxml-serializer)
(use coq-au-vin)
(use (prefix cav-db db:))
(use cav-db-sqlite)
(use (prefix civet cvt:))
(use sxml-transforms)
(use sxml-serializer)

(activate-sqlite)

(let* ((demo-root "examples/demo-site1")
       (data-path (make-pathname demo-root "data"))
       (file (make-pathname data-path "preloaded.db")))
  (db:current-connection (open-database file))
  (db:db-file file)
  (db:content-path (make-pathname data-path "content"))
  (cvt:*site-path* (make-pathname demo-root "dynamic")))

(define date-format #f)
(define %default-date-format% (make-parameter #f))

(define %current-articles% (make-parameter '()))
(define (add-article id)
  (%current-articles% (reverse (cons id (%current-articles%)))))

(define (prepare-article-vars article-data)
  (foldl
    (lambda (prev pair)
      (let ((key (car pair))
            (val (cdr pair)))
        (case key
          ((authors)
           (cons (cons 'authors val) prev))
          ((created_dt)
           (let* ((fmt (or date-format (%default-date-format%)))
                  (dtstring (time->string (seconds->local-time val) date-format)))
             (cons
               (cons 'created_dt dtstring)
               (cons
                 (cons 'raw_dt val)
                 prev))))
          ((title)
           (cons (cons 'article_title val) prev))
          ((content)
           (cons (cons 'article_body (process-body article-data)) prev))
          (else
            (let ((res (if (null? val) (cons key "") pair)))
              (cons res prev))))))
    '()
    article-data))

(define (get-page-vars #!optional (id/alias #f))
  (let ((common-vars
          '((jquerySrc . "/scripts/jquery.js") (urlScheme . "http") (hostName . "quahog")  (bodyMD . "")
            (canEdit . #t) (copyright_year . 2013) (copyright_holders . "Madeleine C St Clair")
            (rights_statement . "You have no rights") (htmlTitle . "Civet Page!") (bodyClasses . ""))))
    (if id/alias
      (cons `(articleID . ,id/alias) common-vars)
      common-vars)))

(define (get-article-ctx id/alias)
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data)))
         (vars (prepare-article-vars article-data))
         ;;; TEMPORARY!
         (page-vars (get-page-vars id/alias))
         (vars* (append page-vars vars)))
    (cvt:make-context vars: vars*)))

(define (get-article-sxml ctx)
  (cvt:process-template-set "article.html" ctx))

(define (get-article-list-ctx #!key (tag #f) (author #f) (limit 10))
  (let-values (((count list-data) ((db:get-articles) mk-teaser: text->teaser tag: tag author: author limit: limit)))
    (for-each
      (lambda (datum)
        (let ((id (alist-ref 'node_id datum)))
          (add-article id)))
      list-data)
    (let* ((list-vars (map prepare-article-vars list-data))
           (page-vars (get-page-vars))
           (vars* (cons (cons 'articles list-vars) page-vars)))
      (cvt:make-context vars: vars*))))

(define (get-article-list-sxml ctx)
  (cvt:process-template-set "article-list.html" ctx))

(define conversion-rules
  `((*text* . ,(lambda (tag body)
                 (string->goodHTML (->string body))))
    . ,universal-conversion-rules*))

(define (sxml->string tree)
  (with-output-to-string
    (lambda ()
      (SRV:send-reply (pre-post-order* tree conversion-rules)))))

(define (write-html tree file)
  (with-output-to-file file
    (lambda () (serialize-sxml tree output: (current-output-port)))))
