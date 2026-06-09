(setq package-enable-at-startup nil)

;; Raise GC threshold during startup; restored after init
(setq gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-percentage 0.6)

;; Suppress file-name-handler-alist during startup for faster require/load
(defvar file-name-handler-alist--saved file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist file-name-handler-alist--saved)))
