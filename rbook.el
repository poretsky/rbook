;;; rbook.el --- reading text files

;; Copyright (C) 2002  Dmitry V. Paduchikh

;; Author: Dmitry Paduchikh <paduch@imm.uran.ru>
;; Modified by Igor B. Poretsky <root@goga.energo.ru>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

;;; Command to read buffer sentence by sentence

(eval-when-compile (require 'cl))
(require 'bookmark)

(defvar rbook-delay-lines 3
  "*Maximum number of empty lines producing pause.")

(defvar rbook-delay-factor 20
  "*One empty line silence length in hundredth of second.")


(defun rbook-delay (n)
  "Makes silence for empty lines."
  (call-process rbook-tts-program nil nil nil "-s"
		(number-to-string
		 (* rbook-delay-factor (min n rbook-delay-lines)))))


(defvar rbook-tts-program "/usr/local/bin/speak"
  "Program used by rbook for tts sakes.
These program should accept text on stdin and produce speech output.")

(defun rbook-speak-region (start end)
  (let ((buf (get-buffer-create " *book*"))
	(oldbuf (current-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert-buffer-substring oldbuf start end)
      ;; Cleaning the text
      (goto-char (point-min))
      (while (re-search-forward "\\(\\w\\)-[ \t]*\n[ \t]*\\(\\w\\)" nil t)
	(replace-match "\\1\\2" nil nil))
      (subst-char-in-region (point-min) (point-max) ?\n ?  t)
      (goto-char (point-max))
      (insert "\n")
      ;; Now speak it
      (call-process-region (point-min) (point-max)
			   rbook-tts-program
			   nil nil nil))))


(defvar rbook-sentence-end "[.?!][]\"')}]*\\([ \t]+\\|[ \t]*\n\\)"
  "*Regular expression to match end of a sentence.")

(defun rbook-read-text ()
  (interactive)
  (dtk-stop)
  (sleep-for 1)
  (push-mark nil t)
  (let ((sentence-end rbook-sentence-end)
	(start (point)) end)
    (while (< start
	      (setq end
		    (save-excursion
		      (forward-sentence 1)
		      (point))))
      (rbook-speak-region start end)
      (setq start end)
      (goto-char start)
      (when (looking-at "[ \t\f]*\n\\([ \t\f]*\n\\)+")
	(goto-char (setq start (match-end 0)))
	(rbook-delay (1- (count-lines (match-beginning 0)
				      (match-end 0))))))))


(defvar rbook-default-bookmark "book"
  "*Default bookmark to start reading from.")


(defun rbook-read-bookmark (bmk)
  "Read text sentence by sentence by feeding it to the program
specified by the variable `rbook-tts-program'. Before reading jump to
the bookmark BMK. When called interactively and prefix argument is
given, ask for BMK. Otherwise use `rbook-default-bookmark' as value."
  (interactive
   (if current-prefix-arg
       (bookmark-completing-read "Read from bookmark"
				 rbook-default-bookmark)
     (list rbook-default-bookmark)))
  (setq rbook-default-bookmark bmk)
  (unless (string= bookmark-current-bookmark bmk)
    (bookmark-jump bmk))
  (unwind-protect
      (rbook-read-text)
    (bookmark-set bmk)))


(provide 'rbook)
;;; rbook.el ends here
