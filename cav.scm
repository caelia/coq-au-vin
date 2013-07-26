(use getopt-long)

(define usage-template
  #<<TEMPLATE
Usage: ~A COMMAND [OPTIONS] [PATH]

Commands:
  ~A create
  ~A update
  ~A delete
  ~A add-templates
  ~A delete-templates
  ~A cache-templates

See '~A help COMMAND' for more information on a specific command.

TEMPLATE
)

(define (show-usage)
  (let ((progname (car (argv))))
    (fprintf
      (current-error-port) 
      usage-template
      progname progname progname progname progname progname progname progname)))

