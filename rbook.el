;;; rbook.el --- reading text files

;; Copyright (C) 2002  Dmitry V. Paduchikh

;; Author: Dmitry Paduchikh <paduch@imm.uran.ru>
;; Modified by Igor B. Poretsky <poretsky@mlbox.ru>
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

;; Requirements:

(eval-when-compile (require 'cl))
(require 'bookmark)


;; Customizations

(defgroup rbook nil
  "Rbook: Book reader"
  :prefix "rbook-"
  :group 'applications)

(defgroup rbook-speech nil
  "Speech parameters"
  :prefix "rbook-speech-"
  :group 'rbook)

(defcustom rbook-speech-volume 0.8
  "Generated speech volume. Reasonable values range from 0.0 to 1.0.
For audio book producing this parameter has no effect."
  :group 'rbook-speech
  :type 'number)

(defcustom rbook-speech-pitch 0.0
  "Generated speech pitch. Reasonable values range from 0.0 to 1.0.
The greater value causes higher pitch."
  :group 'rbook-speech
  :type 'number)

(defcustom rbook-speech-rate 0.5
  "Generated speech rate. Reasonable values range from 0.0 to 1.0.
The greater value causes slower speech."
  :group 'rbook-speech
  :type 'number)

(defcustom rbook-speech-sampling 10300
  "*How to treat generated speech sampling rate. In fact it doesn't affect
actual sampling rate, but varying this value we can change playing speed."
  :group 'rbook-speech
  :type 'integer)

(defvar rbook-default-pronunciation-dictionary "/usr/local/share/ru_tts/lexicon"
  "File name of pronunciation dictionary used by `rbook-tts-program'
if it is not customized by user.")

(defcustom rbook-pronunciation-dictionary rbook-default-pronunciation-dictionary
  "File name of pronunciation dictionary used by `rbook-tts-program'."
  :group 'rbook-speech
  :type '(radio (const :tag "No dictionary" nil)
                (file :must-match t)))

(defgroup rbook-audiobook nil
  "Producing audio books"
  :prefix "rbook-"
  :group 'rbook)

(defcustom rbook-split-by-blank-lines 0
  "If this value is greater than 0, it defines number of blank lines
which causes splitting output. 0 denotes no splitting by blank lines.
Lines filled by non-pronounceable symbols (such as punctuations)
are also treated as blank."
  :group 'rbook-audiobook
  :type 'integer)

(defcustom rbook-split-by-time nil
  "Whether to split output by playing time."
  :group 'rbook-audiobook
  :type 'boolean)

(defcustom rbook-output-chunk-limit 300
  "Time limit of output chunks in seconds.
When splitting output is not requested, this value defines
how frequently the progress message should be displayed."
  :group 'rbook-audiobook
  :type 'integer)

(defcustom rbook-chapter-regexp "[ \f\t]*\\(ГЛАВА[ \f\t]+\\)?[0-9.]+\\(\\w*[ \f\t!-?]*\\)*"
  "Regexp to match chapter header."
  :group 'rbook-audiobook
  :type 'regexp)

(defcustom rbook-mp3-bitrate 32
  "Encoding bitrate in kbps."
  :group 'rbook-audiobook
  :type 'integer)

(defcustom rbook-mp3-volume 1.0
  "Volume scale factor for produced mp3-files."
  :group 'rbook-audiobook
  :type 'number)

(defcustom rbook-eliminate-punctuations t
  "Whether to iliminate speaking of some punctuations,
such as quotes, parentheses and so on."
  :group 'rbook
  :type 'boolean)

(defcustom rbook-delay-lines 3
  "Maximum number of empty lines producing pause."
  :group 'rbook
  :type 'integer)

(defcustom rbook-delay-factor 20
  "One empty line silence length in hundredth of second."
  :group 'rbook
  :type 'integer)

(defcustom rbook-default-bookmark "book"
  "Default bookmark to start reading from."
  :group 'rbook
  :type 'string)

(defcustom rbook-collect-unknown-words nil
  "Whether to allow speech synthesizer to collect unknown words."
  :group 'rbook
  :type '(choice (const :tag "Off" nil)
                 file))

(defun rbook-customize ()
  "Customize Rbook parameters."
  (interactive)
  (customize-group 'rbook))


;; File names and paths

(defvar rbook-scripts-directory (file-name-directory load-file-name)
  "Directory where helper scripts are placed.")

(defvar rbook-sounds-directory (file-name-directory load-file-name)
"Directory where sound icons are stored.")

(defvar rbook-output-files-extension ".mp3"
  "Extension to use for output files.")

(defvar rbook-tts-program (expand-file-name "speak" rbook-scripts-directory)
  "Program used by rbook for tts sakes.
These program should accept text on stdin and produce speech output.")

(defvar rbook-mp3-program (expand-file-name "mp3" rbook-scripts-directory)
  "Program used by rbook for producing audio book in MP3 format.
These program should accept sound stream on stdin and produce an mp3-file.")

(defvar rbook-sound-play-program (expand-file-name "play" rbook-scripts-directory)
  "Program used to play audio icons.")

(defvar rbook-encoding-done-sound (expand-file-name "task-done.au" rbook-sounds-directory)
  "Sound file to play when encoding process finishes.")

(defvar rbook-encoding-progress-sound (expand-file-name "progress.au" rbook-sounds-directory)
  "Sound file to play with progress messages.")


;; TTS input requirements

(defvar rbook-tts-coding-system 'cyrillic-koi8
  "Coding system accepted by the TTS program.")

(defvar rbook-tts-downcase-required t
  "Downcase text before sending it to the TTS program.")


;; Helper functions

(defun rbook-tts-args ()
  "Construct arguments list for the TTS program."
  (let ((args (list
	       "-v" (number-to-string rbook-speech-volume)
	       "-p" (number-to-string rbook-speech-pitch)
	       "-r" (number-to-string rbook-speech-rate)
	       "-f" (number-to-string rbook-speech-sampling))))
    (when (and rbook-pronunciation-dictionary
               (file-readable-p rbook-pronunciation-dictionary))
      (nconc args (list "-d" rbook-pronunciation-dictionary)))
    (when rbook-collect-unknown-words
      (nconc args (list "-l" rbook-collect-unknown-words)))
    args))

(defun rbook-speak ()
  "Speak current buffer."
  (when rbook-tts-downcase-required
    (downcase-region (point-min) (point-max)))
  (let ((coding-system-for-write rbook-tts-coding-system))
    (apply 'call-process-region (point-min) (point-max)
           rbook-tts-program
           nil nil nil (rbook-tts-args))))

(defvar rbook-tts-function 'rbook-speak
  "Function to use for TTS.")

(defun rbook-delay (n)
  "Makes silence for empty lines."
  (call-process rbook-tts-program nil nil nil "-s"
		(number-to-string
		 (* rbook-delay-factor (min n rbook-delay-lines)))))

(defun rbook-speak-region (start end)
  "Transfer text to the TTS engine."
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (erase-buffer)
      (insert-buffer-substring oldbuf start end)
      ;; Searching and parsing Roman numerals
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\|\\W\\)\\([IVXLCDM]+\\)\\(\\W\\|$\\)" nil t)
	(let ((value 0)
	      (ninc 0)
	      (ndec 0)
	      (incs '((?I 1 1 0)
		      (?I 1 2 0)
		      (?I 1 3 0)
		      (?V 5 4 0)
		      (?X 10 5 0)
		      (?X 10 6 1)
		      (?X 10 7 1)
		      (?L 50 8 1)
		      (?C 100 9 1)
		      (?C 100 10 2)
		      (?C 100 11 2)
		      (?D 500 12 2)
		      (?M 1000 13 2)
		      (?M 1000 14 3)
		      (?M 1000 15 3)))
	      (decs '((?I 1 1)
		      (?X 10 2)
		      (?C 100 3))))
	  (goto-char (match-end 2))
	  (while (and value
		      (< (match-beginning 2) (point)))
	    (let ((diginfo (cdr (assoc (preceding-char) (nthcdr ninc incs)))))
	      (if (not diginfo)
		  (let ((diginfo (cdr (assoc (preceding-char) (nthcdr ndec decs)))))
		    (if (not (and diginfo
				  (or (= 5 (/ value (car diginfo)))
				      (and (= 10 (/ value (car diginfo)))
					   (setq ninc (1- ninc))))))
			(setq value nil)
		      (setq value (- value (car diginfo)))
		      (setq ndec (nth 1 diginfo))))
		(setq value (+ value (car diginfo)))
		(setq ninc (nth 1 diginfo))
		(setq ndec (nth 2 diginfo))))
	    (backward-char))
	  (if (not value)
	      (goto-char (match-end 2))
	    (replace-match (concat "\\1"
				   (number-to-string value)
				   "\\3")
			   nil nil)
	    (goto-char (match-beginning 2)))))
      ;; Cleaning the text
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]-\\)[ \t]*\n[ \t]*\\([0-9]\\)" nil t)
	(replace-match "\\1\\2" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\w\\)-[ \t]*\n[ \t]*\\(\\w\\)" nil t)
	(replace-match "\\1\\2" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\w\\)[ \t]*\\(ж\\W\\)" nil t)
	(replace-match "\\1\\2" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(^\\|\\W\\)[БВ]\\)\\(\\.\\)" nil t)
	(replace-match "\\1э\\3" nil nil)
	(backward-char))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(^\\|\\W\\)К\\)\\(\\.\\)" nil t)
	(replace-match "\\1а\\3" nil nil)
	(backward-char))
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\|\\W\\)\\(С\\.\\)" nil t)
	(replace-match "\\1э\\2" nil nil)
	(backward-char))
      (when rbook-eliminate-punctuations
	(subst-char-in-region (point-min) (point-max) ?* ?  t)
	(subst-char-in-region (point-min) (point-max) ?\" ?  t)
	(subst-char-in-region (point-min) (point-max) ?\( ?  t)
	(subst-char-in-region (point-min) (point-max) ?\) ?  t))
      (subst-char-in-region (point-min) (point-max) ?\n ?  t)
      (goto-char (point-min))
      (while (re-search-forward "\\.\\.+" nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "[ \f\t]+" nil t)
	(replace-match " " nil nil))
      (goto-char (point-min))
      (when (re-search-forward "^ " nil t)
	(replace-match "" nil nil))
      (when (re-search-forward " $" nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\)\\( +[0-9]\\)" nil t)
	(replace-match "\\1,\\2" nil nil))
      (goto-char (point-max))
      (insert "\n")
      ;; Now speak it
      (funcall rbook-tts-function))))


(defvar rbook-sentence-end "[.?!]+[]\"')}]*\\([ \t]+\\|[ \t]*\n\\)"
  "*Regular expression to match end of a sentence.")

(defvar rbook-blank-space "[ \t\f!-/:-?]*\n\\([ \t\f!-/:-?]*\n\\)+\\([ \t\f.!?]*[ \t\f]\\)?"
  "Regular expression for blank space detecting.")

(defvar rbook-continue-encoding nil
  "Whether to continue encoding process.")

(defvar rbook-processed-amount 0.0
  "Amount of sound stream already processed.")

(defvar rbook-processing-buffer nil
  "Buffer holding processing text.")

(defvar rbook-current-position 0
  "Current position in the processing text.")

(defvar rbook-output-path nil
  "Path to the file or directory where output should go.")

(defvar rbook-current-output-chunk nil
  "Number of current output chunk or nil.")

(defvar rbook-current-chunk-start-position 0.0
  "Start position of current chunk in the produced sound stream.")

(defvar rbook-tts-process nil
  "TTS process handle.")

(defvar rbook-encoding-process nil
  "Sound encoding process handle.")

(defvar rbook-exchange-buffer nil
  "Buffer for communication of processes.")

(defvar rbook-encoding-process-ready nil
  "Indicates whether encoding process is ready to accept input.")

(defvar rbook-switch-chunk nil
  "Indicates whether to switch chunk.")

(defvar rbook-split-enabled nil
  "Indicates when splitting by chapter may be done.")

(defun rbook-read-sentence ()
  "Read current sentence and process it."
  (let ((sentence-end rbook-sentence-end)
	(start (point)) end)
    (setq end
	  (save-excursion (forward-sentence 1)
			  (point)))
    (if (< start end)
	(if (save-excursion (goto-char start)
			    (re-search-forward "\\w\\|[0-9]" end t))
	    (rbook-speak-region start end)
	  (funcall rbook-tts-function 1))
      (funcall rbook-tts-function rbook-delay-lines)
      (setq rbook-continue-encoding nil))
    (goto-char end)))

(defun rbook-output-file ()
  "Return current output file name."
  (if rbook-current-output-chunk
      (format "%s%03d%s"
	      rbook-output-path
	      rbook-current-output-chunk
	      rbook-output-files-extension)
    rbook-output-path))

(defun rbook-test-current-chunk-length ()
  "Return t if current output chunk exceeds time limit, nil otherwise."
  (>= (/ (- rbook-processed-amount rbook-current-chunk-start-position)
	 rbook-speech-sampling)
      rbook-output-chunk-limit))

(defun rbook-evaluate-time ()
  "Calculate playing time of processed sound."
  (let* ((time (round (/ rbook-processed-amount rbook-speech-sampling)))
	 (hours (/ time 3600))
	 (minutes (/ (% time 3600) 60))
	 (seconds (% time 60)))
    (format "%02d:%02d:%02d" hours minutes seconds)))

(defun rbook-evaluate-size ()
  "Calculate size of produced sound files in kb."
  (round (/ (* rbook-processed-amount rbook-mp3-bitrate)
	    (* rbook-speech-sampling 8))))

(defun rbook-show-time ()
  "Show playing time of already processed text."
  (interactive)
  (message "Processed %s of playing time." (rbook-evaluate-time)))

(defun rbook-run-tts-process (&optional silence)
  "Run TTS process for current buffer.
If optional argument silence is supplied, this process will
generate silence for given number of empty lines."
  (setq rbook-tts-process
	(let ((process-connection-type nil))
	  (apply 'start-process "rbook-tts" rbook-exchange-buffer
		 rbook-tts-program "-n"
		 (if silence
		     (list "-s"
			   (number-to-string
			    (* rbook-delay-factor
			       (min silence rbook-delay-lines))))
		   (rbook-tts-args)))))
  (set-process-coding-system rbook-tts-process 'binary rbook-tts-coding-system)
  (set-process-sentinel
   rbook-tts-process
   (lambda (proc str)
     (when rbook-encoding-process-ready
       (with-current-buffer rbook-exchange-buffer
	 (when (< (point-min) (point-max))
	   (let ((start (point-min))
		 (end (point-max)))
             (when rbook-tts-downcase-required
               (downcase-region start end))
	     (process-send-region rbook-encoding-process start end)
	     (setq rbook-processed-amount
		   (+ rbook-processed-amount (float (- end start))))
	     (delete-region start end))))
       (when rbook-switch-chunk
	 (setq rbook-switch-chunk nil)
	 (setq rbook-encoding-process-ready nil)
	 (process-send-eof rbook-encoding-process))
       (unless rbook-current-output-chunk
	 (when (rbook-test-current-chunk-length)
	   (call-process rbook-sound-play-program
			 nil 0 nil
			 rbook-encoding-progress-sound)
	   (setq rbook-current-chunk-start-position rbook-processed-amount)
	   (rbook-show-time))))
     (unless (buffer-live-p rbook-processing-buffer)
       (setq rbook-continue-encoding nil))
     (if rbook-continue-encoding
	 (with-current-buffer rbook-processing-buffer
	   (goto-char rbook-current-position)
	   (if (looking-at rbook-blank-space)
	       (let ((blank (1- (count-lines
				 (progn (goto-char (match-beginning 0))
					(line-end-position))
				 (progn (goto-char (match-end 0))
					(line-beginning-position))))))
		 (when rbook-current-output-chunk
		   (if (looking-at rbook-chapter-regexp)
		       (progn
			 (when rbook-split-enabled
			   (setq rbook-switch-chunk t))
			 (setq rbook-split-enabled nil))
		     (when (and rbook-split-enabled
				(or (and (> rbook-split-by-blank-lines 0)
					 (>= blank rbook-split-by-blank-lines))
				    (and rbook-split-by-time
					 rbook-encoding-process-ready
					 (rbook-test-current-chunk-length))))
		       (setq rbook-switch-chunk t))
		     (setq rbook-split-enabled t)))
		 (rbook-run-tts-process
		  (if rbook-switch-chunk
		      rbook-delay-lines
		    blank)))
	     (rbook-read-sentence))
	   (setq rbook-current-position (point)))
       (process-send-eof rbook-encoding-process))))
  (unless silence
    (when rbook-tts-downcase-required
      (downcase-region (point-min) (point-max)))
    (process-send-region rbook-tts-process (point-min) (point-max))
    (process-send-eof rbook-tts-process)))

(defun rbook-run-encoding-process (file)
  "Run encoding process."
  (setq rbook-encoding-process
	(let ((process-connection-type nil))
	  (start-process "rbook-encoding" nil
			 rbook-mp3-program
			 "-b" (number-to-string rbook-mp3-bitrate)
			 "-f" (number-to-string
			       (/ rbook-speech-sampling 1000.0))
			 "-v" (number-to-string rbook-mp3-volume)
			 file)))
  (set-process-coding-system rbook-encoding-process 'binary 'binary)
  (set-process-sentinel
   rbook-encoding-process
   (lambda (proc str)
     (if (and rbook-continue-encoding rbook-current-output-chunk)
	 (progn (setq rbook-current-output-chunk
		      (1+ rbook-current-output-chunk))
		(rbook-run-encoding-process (rbook-output-file))
		(call-process rbook-sound-play-program
			      nil 0 nil
			      rbook-encoding-progress-sound)
		(rbook-show-time))
       (call-process rbook-sound-play-program
		     nil 0 nil
		     rbook-encoding-done-sound)
       (kill-buffer rbook-exchange-buffer)
       (setq rbook-encoding-process nil)
       (message
	"Book encoding has %sTotal playing time = %s (%d kb)."
	str (rbook-evaluate-time) (rbook-evaluate-size)))))
  (setq rbook-current-chunk-start-position rbook-processed-amount)
  (setq rbook-encoding-process-ready t))

(defun rbook-construct-output-path (need-split)
  "Construct path to the output file (or directory if NEED-SPLIT)."
  (let ((path (file-name-sans-extension
	       (file-name-sans-extension (buffer-file-name)))))
    (if need-split
	(file-name-as-directory path)
      (concat (directory-file-name path) rbook-output-files-extension))))

(defun rbook-make-audio-book (&optional split path)
  "Make sound files from the text in the current buffer from current position.
If argument SPLIT is not nil then output will be split into separate chunks.
In this case SPLIT denotes the number of the first chunk."
  (interactive
   (let* ((default-chunk-number (or rbook-current-output-chunk 0))
	  (need-split
	   (and (y-or-n-p "Do you need to split output? ")
                (read-number "First chunk number: " default-chunk-number)))
          (path (rbook-construct-output-path need-split)))
     (list need-split
	   (read-file-name "Where output should go? " path path))))
  (when rbook-encoding-process
    (error "Can run only one such process at a time. Sorry"))
  (setq rbook-output-path
        (expand-file-name (or path (rbook-construct-output-path split))))
  (if (not split)
      (setq rbook-output-path (directory-file-name rbook-output-path))
    (setq rbook-output-path (file-name-as-directory rbook-output-path))
    (unless (file-exists-p (directory-file-name rbook-output-path))
      (make-directory rbook-output-path t))
    (unless (file-directory-p rbook-output-path)
      (error "Cannot create directory %s" rbook-output-path)))
  (unless (file-writable-p rbook-output-path)
    (error "Cannot write to %s" rbook-output-path))
  (setq rbook-current-output-chunk split)
  (setq rbook-split-enabled nil)
  (setq rbook-processing-buffer (current-buffer))
  (setq rbook-exchange-buffer (get-buffer-create " *rbook-exchange*"))
  (with-current-buffer rbook-exchange-buffer
    (erase-buffer)
    (set-buffer-multibyte nil))
  (setq rbook-current-position (point))
  (setq rbook-processed-amount 0.0)
  (setq rbook-continue-encoding t)
  (setq rbook-tts-function 'rbook-run-tts-process)
  (rbook-run-encoding-process (rbook-output-file))
  (rbook-read-sentence)
  (setq rbook-current-position (point)))

(defun rbook-read-here ()
  (interactive)
  (setq rbook-tts-function 'rbook-speak)
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
      (when (looking-at rbook-blank-space)
	(rbook-delay (1- (count-lines
			  (progn (goto-char (match-beginning 0))
				 (line-end-position))
			  (progn (goto-char (match-end 0))
				 (line-beginning-position)))))
	(setq start (point))))))

(defvar rbook-last-bookmark nil)

(defun rbook-read-from-bookmark (bmk)
  "Read text sentence by sentence by feeding it to the program
specified by the variable `rbook-tts-program'. Before reading
jump to the bookmark BMK. When called interactively and prefix
argument is given, ask for BMK. Otherwise use last read bookmark
or `rbook-default-bookmark' as value."
  (interactive
   (progn
     (bookmark-maybe-load-default-file)
     (let ((default (or rbook-last-bookmark rbook-default-bookmark)))
       (list
        (if current-prefix-arg
            (bookmark-completing-read "Read from bookmark" default)
          default)))))
  (setq rbook-last-bookmark bmk)
  (unless (string= bookmark-current-bookmark bmk)
    (bookmark-jump bmk))
  (unwind-protect
      (rbook-read-here)
    (bookmark-set bmk)))


(provide 'rbook)
;;; rbook.el ends here
