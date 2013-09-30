(use extras)
(use utf8-srfi-13)
(use coq-au-vin)
(use (prefix cav-db db:))

;(define (get-input tag prompt)
(define (get-input prompt)
  (printf "~A: " prompt)
  (read-line))
  ;(let ((input (read-line)))
    ;`(,tag . ,input)))

;(define (get-secret tag prompt)
(define (get-secret prompt)
  (printf "~A: " prompt)
  (system "stty -echo")
  (let ((input1 (read-line)))
    (newline)
    (system "stty echo")
    (printf "Confirm ~A: " (string-downcase prompt))
    (system "stty -echo")
    (let ((input2 (read-line)))
      (newline)
      (system "stty echo")
      (if (string=? input1 input2)
        input1
        ;`(,tag . ,input1)
        (begin
          (print "Both entries must match.")
          ;(get-secret tag prompt))))))
          (get-secret prompt))))))

(define (create-user)
  (let ((uname (get-input 'uname "Username"))
        (password (get-secret 'password "Password"))
        (email (get-input 'email "Email"))
        (role (get-input 'role "Role"))
        (dispname (get-input 'dispname "Display Name"))
        (blank? (lambda (s) (string=? (string-trim-both s) ""))))
    (cond
      ((or (blank? uname) (blank? password) (blank? email))
       (error "User name, password, and email are required fields."))
      ((not (member role '("admin" "editor" "author" "member" "guest")))
       (error "Role must be one of 'admin', 'editor', 'author', 'member', or 'guest'."))
      (else
        (let ((phash (string->sha1sum password)))
          (if (blank? dispname)
            ((db:add-user) uname phash email role)
            ((db:add-user) uname phash email role dispname)))))))

(define (run)
  ((db:connect))
  (register-roles)
  (create-user)
  ((db:disconnect)))
