(define-record-type
  :sxml-template
  (*make-sxml-template sxml transformed-sxml)
  sxml-template?
  (sxml sxml-template:sxml)
  (transformed-sxml sxml-template:transformed-sxml))

(define (sxml-template:filled? sxml-template)
  (sxml-template:transformed-sxml sxml-template))

(define (make-sxml-template sxml #!key (transformed-sxml #f))
  (*make-sxml-template sxml transformed-sxml))

(define (make-data-rule alist)
  `(%data
    *macro* .
    ,(lambda (tag content)
       (lambda ()
         (let ((value (alist-ref content alist)))
           (cond ((sxml-template? value)
                  (display (sxml-template->string value)))
                 ((list? value)
                  (SRV:send-reply
                   (pre-post-order value universal-conversion-rules)))
                 ((not value)
                  (error (string-append
                          "Value missing for key: "
                          (symbol->string content))))
                 (else (display value))))))))

(define (sxml-template:fill sxml-template alist)
  (let* ((sxml (sxml-template:sxml sxml-template))
	 (transformed-sxml
          (pre-post-order
	    sxml
           `(,(make-data-rule alist)
             (*text* . ,(lambda (tag content) content))
             (*default* . ,(lambda data data)))))
         (transformed-sxml
          (pre-post-order
           transformed-sxml
           universal-conversion-rules)))
    (make-sxml-template
     sxml transformed-sxml: transformed-sxml)))

(define (sxml-template->string sxml-template)
  (if (sxml-template:filled? sxml-template)
      (with-output-to-string
        (lambda () (SRV:send-reply
                    (sxml-template:transformed-sxml sxml-template))))
      (error "Unfilled SXML template")))

(define sxml-template:fill-string
  (compose sxml-template->string sxml-template:fill))
