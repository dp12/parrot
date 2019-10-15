;;; parrot-rotate.el --- Parrot rotates words with smooth circular motions.  -*- lexical-binding: t; -*-

;; Author: Daniel Ting <deep.paren.12@gmail.com>
;; URL: https://github.com/dp12/parrot.git
;; Version: 1.1.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: party, parrot, rotate, sirocco, kakapo, convenience

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
;;
;; Portions of this code are copied and modified from Aaron Hawley's rotate text
;; implementation.  Check it out at https://www.emacswiki.org/emacs/RotateText.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar parrot-rotate-dict
  '(
    (:rot ("begin" "end") :caps t :upcase t)
    (:rot ("enable" "disable") :caps t :upcase t)
    (:rot ("enter" "exit") :caps t :upcase t)
    (:rot ("forward" "backward") :caps t :upcase t)
    (:rot ("front" "rear" "back") :caps t :upcase t)
    (:rot ("get" "set") :caps t :upcase t)
    (:rot ("high" "low") :caps t :upcase t)
    (:rot ("in" "out") :caps t :upcase t)
    (:rot ("left" "right") :caps t :upcase t)
    (:rot ("min" "max") :caps t :upcase t)
    (:rot ("on" "off") :caps t :upcase t)
    (:rot ("prev" "next"))
    (:rot ("start" "stop") :caps t :upcase t)
    (:rot ("true" "false") :caps t :upcase t)
    (:rot ("&&" "||"))
    (:rot ("==" "!="))
    (:rot ("." "->"))
    (:rot ("if" "else" "elif"))
    (:rot ("ifdef" "ifndef"))
    (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
    (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
    (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
    (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
    )
  "Dictionary of rotations to use.
:rot is the list of rotations.
:caps t (optional) provides the capitalized version of the rotations.
:upcase t (optional) provides the upcased version of the rotations.
:lower nil (optional) excludes the lowercased version of the rotations.")

(defvar pulse-flag nil
  "Non-nil means highlighting should fade away with a timer.
This is set to nil in parrot-rotate so that the highlight from the pulse library
will persist until the next command.")

(defcustom parrot-rotate-hunt-for-words t
  "If non-nil, search for replacements even if your cursor isn't on the word."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'parrot)

(defcustom parrot-rotate-jump-to-word-after-hunt t
  "If non-nil, jump to rotation after replacement on word not under the cursor.
It has no effect if ‘parrot-rotate-hunt-for-words’ is nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'parrot)

(defcustom parrot-rotate-animate-after-rotation t
  "If non-nil, the party parrot will animate when a replacement is made."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'parrot)

(defcustom parrot-rotate-highlight-after-rotation t
  "If non-nil, replaced text will be highlighted after a rotation."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'parrot)

(defcustom parrot-rotate-start-char-invalid-regexp "[[:blank:]]"
  "Regex to check if character under cursor is invalid for starting a rotation."
  :type 'string
  :group 'parrot)

(defcustom parrot-rotate-start-bound-regexp "[[:space:]]"
  "Regex to search backward for text rotation start point.
This should be the point before the start character that will be NOT considered
part of the text rotation scope.  By default, it is set to [[:space:]], so
parrot will search a whitespace-delimited word for potential rotations.  You can
change it to [\]\[[:space:](){}<>] to treat braces/brackets as boundaries."
  :type 'string
  :group 'parrot)

(defcustom parrot-rotate-end-bound-regexp "[[:space:]]"
  "Regex to search forward for text rotation end point.
This should be the point after the end character that will be NOT considered
part of the text rotation scope.  By default, it is set to [[:space:]], so
parrot will search a whitespace-delimited word for potential rotations.  You can
change it to [\]\[[:space:](){}<>] to treat braces/brackets as boundaries."
  :type 'string
  :group 'parrot)

(defface parrot-rotate-rotation-highlight-face
  '((t (:inherit highlight)))
  "Face used for highlighting rotations."
  :group 'parrot)

(defun parrot-rotate-convert-rotations-to-regexp (rotations)
  "Return regular expressions for all entries in ROTATIONS.
ROTATIONS contains lists of strings with optional :lower, :caps, or :upcase
labels.  The regular expression returned checks for a match with any one of the
strings in the entire rotation list."
  (regexp-opt
   (cl-mapcan
    (lambda (entry)
      (let ((entry-rotations
             (append
              (unless (and (plist-member entry :lower) (not (plist-get entry :lower)))
                (plist-get entry :rot))
              (when (plist-get entry :caps)
                (mapcar #'capitalize (plist-get entry :rot)))
              (when (plist-get entry :upcase)
                (mapcar #'upcase (plist-get entry :rot))))))
        (unless entry-rotations
          (error "%S has no rotations" (plist-get entry :rot)))
        (unless (> (length (plist-get entry :rot)) 1)
          (error "%S must have at least two rotations" (plist-get entry :rot)))
        entry-rotations))
    rotations)))

(defun parrot-rotate-get-rots-for (string)
  "Return the string rotations for STRING.
Given STRING, this function returns a list of the rotations available for it, as
defined in `parrot-rotate-dict`.  If the :caps or :upcase labels are specified
and the string is capitalized or upcased, the corresponding capitalized or
upcased versions of the rotations will returned."
  (remq nil
        (mapcar
         (lambda (entry)
           (let ((rot (plist-get entry :rot))
                 (caps (plist-get entry :caps))
                 (upcase (plist-get entry :upcase)))
             (cond ((member string rot) rot)
                   ((and caps (member string (mapcar #'capitalize rot)))
                    (mapcar #'capitalize rot))
                   ((and upcase (member string (mapcar #'upcase rot)))
                    (mapcar #'upcase rot)))))
         parrot-rotate-dict)))

(defun parrot-rotate-prev (string)
  "Return the previous element before STRING in ROTATIONS."
  (parrot-rotate-next string t))

(defun parrot-rotate-next (string &optional reverse)
  "Return the next element in the dictionary after STRING.
If REVERSE is specified, the previous item will be returned (equivalent to
calling ‘parrot-rotate-prev’)."
  (let ((rots (parrot-rotate-get-rots-for string)))
    (when (> (length rots) 1)
      (error (format "Ambiguous rotation for %s" string)))
    (if (< (length rots) 1)
        ;; If we get this far, this should not occur
        nil
      (let* ((rot-list (if reverse
                           (reverse (car rots))
                         (car rots)))
             (occurs-in-rots (member string rot-list)))
        (when (null occurs-in-rots)
          ;; If we get this far, this should *never* occur:
          (error (format "Unknown rotation for %s" string)))
        (if (null (cdr occurs-in-rots))
            (car rot-list)
          (cadr occurs-in-rots))))))

(defun parrot-rotate-prev-word-at-point ()
  "Rotate the word at point to the previous word in ‘parrot-rotate-dict’."
  (interactive)
  (parrot-rotate-word-at-point #'parrot-rotate-prev))

(defun parrot-rotate-next-word-at-point ()
  "Rotate the word at point to the next word in ‘parrot-rotate-dict’."
  (interactive)
  (parrot-rotate-word-at-point #'parrot-rotate-next))

(declare-function parrot-start-animation "parrot.el")
(defun parrot-rotate-word-at-point (rotate-func)
  "Rotates the word at point using ROTATE-FUNC."
  (let* ((start-cursor (point))
        (end-cursor start-cursor)
        (word-start nil)
        (word-end-mark nil)
        (regexp (parrot-rotate-convert-rotations-to-regexp parrot-rotate-dict))
        (case-fold-search nil))

    ;; Make sure cursor isn't on an invalid character, e.g. blank character
    (when (looking-at-p parrot-rotate-start-char-invalid-regexp)
      (error "No parrot matches found"))

    (save-excursion
      ;; Find the bounds of the current word
      (if (re-search-forward parrot-rotate-end-bound-regexp (line-end-position) t)
          (progn
            (backward-char)
            (setq word-end-mark (copy-marker (point))))
        (setq word-end-mark (copy-marker (line-end-position))))
      (if (re-search-backward parrot-rotate-start-bound-regexp (line-beginning-position) t)
          (progn
            (forward-char)
            (setq word-start (point)))
        (setq word-start (line-beginning-position)))

      ;; Search the whole word; if the start-cursor is within a match, rotate the match and exit
      (goto-char word-start)
      (let ((did-replace nil))
        (while (and (not did-replace) (re-search-forward regexp (marker-position word-end-mark) t))
          (when (and (< start-cursor (match-end 0)) (> start-cursor (- (match-beginning 0) 1)))
              (replace-match (funcall rotate-func (match-string 0)) t)
              ;; If the replacement text is shorter than original text and the
              ;; cursor would fall off of it after replacement, move back the
              ;; cursor to the end of the replaced word.
              ;; e.g. false| --> true|
              (when (> (- start-cursor (match-beginning 0))
                       (- (- (point) 1) (match-beginning 0)))
                (setq end-cursor (- (point) 1)))
              (setq did-replace t)))

        ;; Cursor is not on a match, look right and left for matches and see
        ;; which match is closer
        (when (and parrot-rotate-hunt-for-words (not did-replace))
          (let ((rmatch nil)
                (lmatch nil)
                (rmatch-start nil)
                (lmatch-end nil)
                (rmatch-data nil))
          (goto-char start-cursor)
          (when (re-search-forward regexp (marker-position word-end-mark) t)
            (setq rmatch (match-string 0))
            (setq rmatch-start (- (match-beginning 0) 1))
            (setq rmatch-data (match-data)))
          (goto-char start-cursor)
          (when (re-search-backward regexp word-start t)
            (setq lmatch (match-string 0))
            (setq lmatch-end (match-end 0)))

          (setq did-replace t)
          (cond
           ;; Case 1: No matches
           ((and (not lmatch) (not rmatch)) (setq did-replace nil))
           ;; Case 2: One match to the left
           ((not rmatch) (progn
                           (replace-match (funcall rotate-func lmatch) t)
                           (when parrot-rotate-jump-to-word-after-hunt
                             (setq end-cursor (- (point) 1)))))
           ;; Case 3: One match to the right
           ((not lmatch) (progn
                           (set-match-data rmatch-data)
                           (replace-match (funcall rotate-func rmatch) t)
                           (when parrot-rotate-jump-to-word-after-hunt
                             (setq end-cursor (+ rmatch-start 1)))))
           ;; Case 4: Matches to the left and right
           (t (if (< (- start-cursor lmatch-end) (- rmatch-start start-cursor))
                  ;; Left match is closer
                  (progn
                    (replace-match (funcall rotate-func lmatch) t)
                    (when parrot-rotate-jump-to-word-after-hunt
                      (setq end-cursor (- (point) 1))))
                ;; Right match is closer
                (set-match-data rmatch-data)
                (replace-match (funcall rotate-func rmatch) t)
                (when parrot-rotate-jump-to-word-after-hunt
                  (setq end-cursor (+ rmatch-start 1))))))))
        (if did-replace
            (progn
              (when (and (fboundp #'pulse-momentary-highlight-region) parrot-rotate-highlight-after-rotation)
                (let ((replace-start (match-beginning 0))
                      (replace-end (point))
                      (pulse-flag nil))
                  (pulse-momentary-highlight-region replace-start replace-end 'parrot-rotate-rotation-highlight-face)))
              (when parrot-rotate-animate-after-rotation
                (parrot-start-animation)))
          (error "No parrot matches found"))))
    (goto-char end-cursor)))

(provide 'parrot-rotate)

;;; parrot-rotate.el ends here
