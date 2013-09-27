(use (prefix cav-db db:))
(use cav-db-sqlite)
(use posix)

(define (setup dbfile content-path)
  (setup-db dbfile)
  (create-directory content-path #t))

; (define (init dbfile)
  ; (let ((qs
          ; '("INSERT INTO roles 

