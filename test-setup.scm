(use (prefix cav-db db:))
(use (prefix sql-de-lite sd:))
(use cav-db-sqlite)
(use posix)
(use simple-sha1)

(define (eprintf . args)
  (error (apply sprintf args)))

(define %dbfile% (make-parameter #f))

(define %cpath% (make-parameter #f))

(define %users% (make-queue))

(define (add-user uname password email role #!optional (dispname '()))
  (let ((phash (string->sha1sum password)))
    (queue-add! %users% (list uname phash email dispname role))))

(define (pop-user)
  (and (not (queue-empty? %users%))
       (queue-remove! %users%)))

(define (init dbfile cpath)
  (%dbfile% dbfile)
  (%cpath% cpath)
  (enable-sqlite dbfile cpath))

(define (setup #!optional (force #f))
  (setup-db (%dbfile%) force)
  (let ((path (%cpath%)))
    (when (file-exists? path)
      (if force
        (delete-directory path #t)
        (eprintf "Path exists: ~A\n" path)))
    (create-directory path #t)))

(define (db-init-roles)
  (sd:call-with-database
    (%dbfile%)
    (lambda (conn)
      (let* ((q "INSERT INTO roles (name) VALUES (?);")
             (st (sd:sql conn q)))
        (for-each
          (lambda (r) (sd:exec st r))
          '("admin" "editor" "author" "member" "guest"))))))

(define (db-add-users)
  (sd:call-with-database
    (%dbfile%)
    (lambda (conn)
      (let* ((q "INSERT INTO users (uname, passhash, email, role, display_name)
                  SELECT ?, ?, ?, roles.id, ? FROM roles
                  WHERE roles.name = ?;")
             (s (sd:sql conn q)))
        (let loop ((u (pop-user)))
          (when u
            (print "Adding user:")
            (pp u)
            (apply sd:exec `(,s ,@u))
            (loop (pop-user))))))))

