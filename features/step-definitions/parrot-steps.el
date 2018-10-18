;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^parrot-rotate-next-word-at-point should throw an error$"
      (lambda ()
        (should-error (parrot-rotate-next-word-at-point))))
