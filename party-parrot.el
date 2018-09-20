;;; party-parrot.el --- Party Parrot rotates gracefully in mode-line.  -*- lexical-binding: t; -*-

;; Author: Daniel Ting <dp12@github.com>
;; URL: https://github.com/dp12/parrot.git
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: party, parrot, rotate, sirocco, kakapo, games

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To activate this mode, add (party-parrot-mode) to your init file.
;; To get the parrot to rotate on new email messages in mu4e, add:
;; (add-hook 'mu4e-index-updated-hook #'party-parrot-start-animation)
;;
;; This animation code is a heavily modified version of Jacek "TeMPOraL"
;; Zlydach's famous nyan-mode. Check out his original work at
;; https://github.com/TeMPOraL/nyan-mode/.

;;; Code:

(eval-when-compile (require 'cl))

(defconst party-parrot-directory (file-name-directory (or load-file-name buffer-file-name)))
(defconst party-parrot-modeline-help-string "mouse-1: Rotate with parrot!")

;; ('v') (*'v') ('V'*) ('v'*)
;; ('v') ('V') ('>') ('^') ('<') ('V') ('v')

(defgroup parrot nil
  "Customization group for `party-parrot-mode'."
  :group 'frames)

(defun party-parrot-refresh ()
  "Refresh after option change if loaded."
  (when (featurep 'party-parrot-mode)
    (when (and (boundp 'party-parrot-mode)
               party-parrot-mode)
      (party-parrot-mode -1)
      (party-parrot-mode 1))))

(defcustom party-parrot-animation-frame-interval 0.045
  "Number of seconds between animation frames."
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (party-parrot-refresh))
  :group 'parrot)

(defvar party-parrot-animation-timer nil)

(defvar party-parrot-rotations 0)

(defun party-parrot-start-animation ()
  "Start the parrot animation."
  (interactive)
  (setq party-parrot-rotations 0)
  (when (not (and party-parrot-animate-party-parrot
                  party-parrot-animation-timer))
    (setq party-parrot-animation-timer (run-at-time nil
                                                    party-parrot-animation-frame-interval
                                                    #'party-parrot-switch-anim-frame))
    (setq party-parrot-animate-party-parrot t)))

(defun party-parrot-stop-animation ()
  "Stop the parrot animation."
  (interactive)
  (when (and party-parrot-animate-party-parrot
             party-parrot-animation-timer)
    (cancel-timer party-parrot-animation-timer)
    (setq party-parrot-animation-timer nil)
    (setq party-parrot-animate-party-parrot nil)))

(defcustom party-parrot-minimum-window-width 45
  "Determines the minimum width of the window, below which party parrot will not be displayed."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (party-parrot-refresh))
  :group 'parrot)

(defcustom party-parrot-animate-party-parrot nil
  "Enable animation for party parrot.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (party-parrot-start-animation)
           (party-parrot-stop-animation))
         (party-parrot-refresh))
  :group 'parrot)

(defcustom party-parrot-spaces-before 0
  "Spaces of padding before parrot in mode line."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (party-parrot-refresh))
  :group 'parrot)

(defcustom party-parrot-spaces-after 0
  "Spaces of padding after parrot in the mode line."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (party-parrot-refresh))
  :group 'parrot)

(defcustom party-parrot-num-rotations 3
  "How many times party parrot will rotate."
  :type 'integer
  :group 'parrot)

(defvar party-parrot-frame-list (number-sequence 1 10))
(defvar party-parrot-type nil)
(defvar party-parrot-static-image nil)
(defvar party-parrot-animation-frames nil)

(defun party-parrot-load-frames (parrot-type)
  "Load the images for the selected PARROT-TYPE."
  (when (image-type-available-p 'xpm)
    (setq party-parrot-static-image (create-image (concat party-parrot-directory (format "img/%s/%s-parrot-frame-1.xpm" parrot-type parrot-type)) 'xpm nil :ascent 'center))
    (setq party-parrot-animation-frames (mapcar (lambda (id)
                                                  (create-image (concat party-parrot-directory (format "img/%s/%s-parrot-frame-%d.xpm" parrot-type parrot-type id))
                                                                'xpm nil :ascent 'center))
                                                party-parrot-frame-list))))

(defun party-parrot-set-parrot-type (parrot)
  "Set the desired PARROT type in the mode line."
  (interactive (list (completing-read "Select parrot: "
                                      '(default confused emacs nyan rotating science thumbsup))))
  (let ((parrot-found t))
    (cond ((string= parrot "default") (setq party-parrot-frame-list (number-sequence 1 10)))
          ((string= parrot "confused") (setq party-parrot-frame-list (number-sequence 1 26)))
          ((string= parrot "emacs") (setq party-parrot-frame-list (number-sequence 1 10)))
          ((string= parrot "nyan") (setq party-parrot-frame-list (number-sequence 1 10)))
          ((string= parrot "rotating") (setq party-parrot-frame-list (number-sequence 1 13)))
          ((string= parrot "science") (setq party-parrot-frame-list (number-sequence 1 10)))
          ((string= parrot "thumbsup") (setq party-parrot-frame-list (number-sequence 1 12)))
          (t (setq parrot-found nil))
          )
    (if (not parrot-found)
        (message (format "Error: no %s parrot available" parrot))
      (setq party-parrot-type parrot)
      (party-parrot-load-frames parrot)
      (run-at-time "0.5 seconds" nil #'party-parrot-start-animation)
      (message (format "%s parrot selected" parrot)))))

(defvar party-parrot-current-frame 0)

(defun party-parrot-switch-anim-frame ()
  "Change to the next frame in the parrot animation.
If the parrot has already
rotated for `party-parrot-num-rotations', the animation will stop."
  (setq party-parrot-current-frame (% (+ 1 party-parrot-current-frame) (car (last party-parrot-frame-list))))
  (when (eq party-parrot-current-frame 0)
    (setq party-parrot-rotations (+ 1 party-parrot-rotations))
    (when (>= party-parrot-rotations party-parrot-num-rotations)
      (party-parrot-stop-animation)))
  (force-mode-line-update))

(defun party-parrot-get-anim-frame ()
  "Get the current animation frame."
  (if party-parrot-animate-party-parrot
      (nth party-parrot-current-frame party-parrot-animation-frames)
    party-parrot-static-image))

(defun party-parrot-add-click-handler (string)
  "Add a handler to STRING for animating the parrot when it is clicked."
  (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (party-parrot-start-animation)))))))

(defun party-parrot-create ()
  "Generate the party parrot string."
  (if (< (window-width) party-parrot-minimum-window-width)
      ""                                ; disabled for too small windows
    (let ((party-parrot-string (make-string party-parrot-spaces-before ?\s)))
      (setq party-parrot-string (concat party-parrot-string (party-parrot-add-click-handler
                                                             (propertize "-" 'display (party-parrot-get-anim-frame)))
                                        (make-string party-parrot-spaces-after ?\s)))
      (propertize party-parrot-string 'help-echo party-parrot-modeline-help-string))))

(defvar party-parrot-old-cdr-mode-line-position nil)

;;;###autoload
(define-minor-mode party-parrot-mode
  "Use Party Parrot to show when you're rotating
You can customize this minor mode, see option `party-parrot-mode'."
  :global t
  :group 'parrot
  :require 'party-parrot
  (if party-parrot-mode
      (progn
        (unless party-parrot-type (party-parrot-set-parrot-type 'default))
        (unless party-parrot-old-cdr-mode-line-position
          (setq party-parrot-old-cdr-mode-line-position (cdr mode-line-position)))
        (setcdr mode-line-position (cons '(:eval (list (party-parrot-create)))
                                         (cdr party-parrot-old-cdr-mode-line-position))))
    (setcdr mode-line-position party-parrot-old-cdr-mode-line-position)))

(provide 'party-parrot)

;;; party-parrot.el ends here
