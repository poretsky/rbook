
;; Rbook
(autoload 'rbook-read-text "rbook" nil t)
(autoload 'rbook-read-bookmark "rbook" nil t)
(autoload 'rbook-make-audio-book "rbook" nil t)
(autoload 'rbook-show-time "rbook" nil t)
(autoload 'rbook-customize "rbook" nil t)
(global-set-key (kbd "C-c b") 'rbook-read-bookmark)
(global-set-key (kbd "C-c r") 'rbook-read-text)
(global-set-key (kbd "C-c a") 'rbook-make-audio-book)
(global-set-key (kbd "C-c t") 'rbook-show-time)
(global-set-key (kbd "C-c c") 'rbook-customize)
