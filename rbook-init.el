;;; rbook-init.el --- Rbook binding

;; Copyright (C) 2010  Igor B. Poretsky

;; Author: Igor B. Poretsky <poretsky@mlbox.ru>

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

;; Autoloads:
(autoload 'rbook-read-here "rbook" nil t)
(autoload 'rbook-read-from-bookmark "rbook" nil t)
(autoload 'rbook-make-audio-book "rbook" nil t)
(autoload 'rbook-show-time "rbook" nil t)
(autoload 'rbook-customize "rbook" nil t)

;; Hot keys:
(global-set-key (kbd "C-c b") 'rbook-read-from-bookmark)
(global-set-key (kbd "C-c r") 'rbook-read-here)
(global-set-key (kbd "C-c a") 'rbook-make-audio-book)
(global-set-key (kbd "C-c t") 'rbook-show-time)
(global-set-key (kbd "C-c c") 'rbook-customize)

;; Menu bindings:
(define-key global-map [menu-bar tools RBOOK] (cons "Book reader" (make-sparse-keymap "RBOOK")))
(define-key global-map [menu-bar tools RBOOK customize] '("Customize parameters" . rbook-customize))
(define-key global-map [menu-bar tools RBOOK show-time] '("Show processed time" . rbook-show-time))
(define-key global-map [menu-bar tools RBOOK make-audio-book] '("Make audio book" . rbook-make-audio-book))
(define-key global-map [menu-bar tools RBOOK read-from-bookmark]
  '("Read text from bookmark" . rbook-read-from-bookmark))
(define-key global-map [menu-bar tools RBOOK read-here] '("Read text from cursor" . rbook-read-here))
