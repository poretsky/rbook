
;; Rbook
(autoload 'rbook-read-text "rbook" nil t)
(autoload 'rbook-read-bookmark "rbook" nil t)
(global-set-key (kbd "C-c r") 'rbook-read-bookmark)
