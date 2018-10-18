(require 'f)

(defvar parrot-support-path
  (f-dirname load-file-name))

(defvar parrot-features-path
  (f-parent parrot-support-path))

(defvar parrot-root-path
  (f-parent parrot-features-path))

(add-to-list 'load-path parrot-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'parrot)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 (setq parrot-rotate-hunt-for-words t)
 (setq parrot-rotate-jump-to-word-after-hunt t))

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
