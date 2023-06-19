;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Python IDE
    ef-themes                       ;; color themes
    ;;neotree                         ;; Tree File Browser
    zoom                            ;; better window splitting
    ace-window                      ;; easier switching between windows
    markdown-mode
    multiple-cursors                ;; multi editing like PyCharm
    expand-region                   ;; nice expansion of selection like in PyCharm
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================
;; Basic Customization
;; ===================================

(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally


;; Load the theme of choice:
(load-theme 'ef-day :no-confirm)
;;(load-theme 'timu-caribbean t)

(setq ido-enable-flex-matching t)       ;;ido mode settings
(setq ido-everywhere t)
(ido-mode 1)

;; Start Emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(initial-buffer-choice "~/.emacs.d/bookmarks/")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors elpy better-defaults))
 '(zoom-mode t nil (zoom)))

 (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;===================================================
;;IDE Stuff
;;===================================================
;; Enable elpy
(elpy-enable)

(setq json-encoding-pretty-print nil)

;; workon home  to select conda environments with Alt+x pyvenv-workon
;; Choosing an conda environment with pyvenv-workon apparently has to to happen before opening the
;; console (starting the python process)
(setenv "WORKON_HOME" "/home/voit/miniconda3/envs/")

;; run line in shell
(define-key elpy-mode-map (kbd "C-r") 'elpy-shell-send-statement-and-step)

;; always highlight matching parenthesis
(show-paren-mode 1)

(require 'multiple-cursors)   ;;multiple editing like in pycharm
(define-key global-map (kbd "M-j") 'mc/mark-next-like-this-word)

(require 'expand-region)           ;; smart expanding selection of expressions
(global-set-key (kbd "C-M-W") 'er/expand-region)

;;(require 'neotree)
;;(global-set-key [f8] 'neotree-toggle)


;;===================================================
;; Buffer and windows
;;===================================================
;;To have it instead open the buffer menu and switch to it in one action, rebind the key as follows:
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

;;To just open the buffer menu in the current window (burying whatever buffer you were in before):
;;(global-set-key (kbd "C-x C-b") 'buffer-menu)

(global-set-key (kbd "<f1>") 'menu-bar-open)


;;===============================================
;; Folder and Sunrise commander settings
;;===============================================
;;(add-to-list 'load-path "~/.emacs.d/sunrise-commander/")
;;(require 'sunrise)
;;(global-set-key (kbd "<f7>") 'sunrise)  ;;to open the file commander


(defun hpc ()
  "Opens the work folder no the HPC by SSH with dired."
  (interactive)
  (dired "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))


(global-set-key (kbd "M-o") 'ace-window)  ;; doesn't seem to work with sunrise commander
;; see here: https://github.com/sunrise-commander/sunrise-commander/issues/108



