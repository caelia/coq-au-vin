;;; coq-au-vin.scm -- A simple blogging platform based on Chicken Scheme.
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module coq-au-vin
        *
        (import scheme)
        (import chicken)
        (import files)
        (import ports)

        (use lowdown)
        (use ersatz)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (strip-html str)
  (let* ((in-tag #f)
         (enter-tag
           (lambda ()
             (if in-tag
               (set! in-tag (+ in-tag 1))
               (set! in-tag 1))))
         (leave-tag
           (lambda ()
             (cond
               ((not in-tag) #f)
               ((= in-tag 1) (set! in-tag #f))
               (else (set! in-tag (- in-tag 1)))))))
    (with-output-to-string
      (lambda ()
        (with-input-from-string
          str
          (lambda ()
            (let loop ((c (read-char)))
              (cond
                ((eof-object? c)
                 #f)
                ((char=? c #\<)
                 (enter-tag)
                 (loop (read-char)))
                ((char=? c #\>)
                 (leave-tag)
                 (loop (read-char)))
                (in-tag
                  (loop (read-char)))
                (else
                  (write-char c)
                  (loop (read-char)))))))))))

(define (escape-html str)
  (with-output-to-string
    (lambda ()
      (with-input-from-string
        str
        (lambda ()
          (let loop ((c (read-char)))
            (cond
              ((eof-object? c)
               #f)
              ((char=? c #\<)
               (display "&lt;")
               (loop (read-char)))
              ((char=? c #\>)
               (display "&gt;"))
               (loop (read-char)))
              (else
                (write-char c)
                (loop (read-char))))))))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  GLOBAL PARAMETERS  -----------------------------------------------

(define %blog-root% (make-parameter #f))

(define %default-teaser-length% (make-parameter 1024))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define (default-teaser article-body)
  (let ((alen (string-length article-body))
        (tlen (%default-teaser-length%)))
    (if (< tlen alen)
      (substring article-body 0 tlen)
      article-body)))

(define (get-template tpl-name #!optional (content-type 'text/html))
  (let* ((file-ext
           (case content-type
             ((text/html) "html")
             ((application/json) "json")
             (else (eprintf "[get-template] Unknown content type: ~A" content-type))))
         (tpl-path
          (make-pathname
            (make-pathname (%blog-root%) "templates")
            tpl-name file-ext)))
    (if (file-exists? tpl-path)
      tpl-path
      (eprintf "[get-template] Template file '~A' not found." tpl-path))))

(define (teaser article-id)
  (or (get-teaser article-id)
      (default-teaser (get-body article-id))))

(define (latest #!optional (n 10))
  (for-each
    (lambda (id) (teaser id))
    (index-ref 'post-order 10)))

(define (init blog-root)
  (%blog-root% blog-root))

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

