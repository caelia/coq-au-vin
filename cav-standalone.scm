;;; cav-standalone.scm -- Source code for standalone executable version.
;;;
;;;   Copyright © 2013 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(include "cav-db.scm")
(include "cav-web.scm")
(include "cav-db-sqlite.scm")
(include "cav-web-spiffy.scm")
(include "coq-au-vin.scm")

(import cav-db-sqlite)

(define (init)
  (activate-sqlite)
  (activate-spiffy))

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

; vim:et:ai:ts=2 sw=2
