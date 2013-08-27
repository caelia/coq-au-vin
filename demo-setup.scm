(load "coq-au-vin.scm")
(import coq-au-vin)
(use (prefix cav-db db:))
(load "cav-db-sqlite.scm")
(import cav-db-sqlite)
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

(define (get-sxml id/alias)
  (let* ((article-data (get-article-data id/alias))
         ; (html-body (process-body article-data))
         (vars
           (foldl
             (lambda (prev pair)
               (let ((key (car pair))
                     (val (cdr pair)))
                 (case key
                   ((authors)
                    (let* ((first (car val))
                           (others (cdr val))
                           (result* (cons (cons 'author first) prev)))
                      (if (null? others)
                        result*
                        (cons (cons 'other_authors others) result*))))
                   ((created_dt)
                    (let* ((fmt (or date-format (%default-date-format%)))
                           (dtstring (time->string (seconds->local-time val) date-format)))
                      (cons (cons 'created_dt dtstring) prev)))
                   ((title)
                    (cons (cons 'article_title val) prev))
                   ((content)
                    (cons (cons 'article_body (process-body article-data)) prev))
                   (else
                     (let ((res (if (null? val) (cons key "") pair)))
                       (cons res prev))))))

             '()
             article-data))
         ;;; TEMPORARY!
         (extra-vars
            `((urlScheme . "http") (hostName . "quahog") (articleID . ,id/alias) (bodyMD . "")
              (canEdit . #t) (copyright_year . 2013) (copyright_holders . "Madeleine C St Clair")
              (rights_statement . "You have no rights") (htmlTitle . "Civet Page!") (bodyClasses . "")))
         (vars* (append extra-vars vars))
         (ctx (cvt:make-context vars: vars*)))
    (cvt:process-template-set "article.html" ctx)))

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
    (lambda ()
      (SRV:send-reply (pre-post-order* tree conversion-rules)))))

(define (write-html2 tree file)
  (with-output-to-file file
    (lambda () (serialize-sxml tree output: (current-output-port)))))
