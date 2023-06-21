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
;;(package-refresh-contents) ;; needed if packages are sometimes not installed properly

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Python IDE
    zoom                            ;; better window splitting
    ;;ace-window                      ;; easier switching between windows
    markdown-mode
    multiple-cursors                ;; multi editing like PyCharm
    expand-region                   ;; nice expansion of selection like in PyCharm
    zenburn-theme
    flycheck                    ;; syntax check for python, seems to work less buggy than flymake
    magit
    centaur-tabs
    all-the-icons              ;;all-the-icons-install-fonts might be needed for the first run (out of emacs)
    all-the-icons-dired    
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
(load-theme 'zenburn t)

(setq ido-enable-flex-matching t)       ;;ido mode settings
(setq ido-everywhere t)
(ido-mode 1)

;; Start Emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "8294b451ffe0575fcccd1a447f56efc94d9560787cd5ff105e620e5f5771427d" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(all-the-icons-dired all-the-icons nerd-icons centaur-tabs magit flycheck color-theme-sanityinc-tomorrow zenburn-theme color-theme multiple-cursors better-defaults))
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

(elpy-enable)

;;exchange flymake to the more modern flycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; workon home  to select conda environments with Alt+x pyvenv-workon
;; Choosing an conda environment with pyvenv-workon apparently has to to happen before opening the
;; console (starting the python process)


(if (equal system-name "n-hpc-login1")
    (setenv "WORKON_HOME" "/home/voit/.conda/envs/")
    (setenv "WORKON_HOME" "/home/voit/miniconda3/envs/"))


;; run line in shell
(define-key elpy-mode-map (kbd "C-r") 'elpy-shell-send-statement-and-step)

;; always highlight matching parenthesis
(show-paren-mode 1)

(require 'multiple-cursors)   ;;multiple editing like in pycharm
(define-key global-map (kbd "M-j") 'mc/mark-next-like-this-word)

(require 'expand-region)           ;; smart expanding selection of expressions
(global-set-key (kbd "C-M-W") 'er/expand-region)



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

(setq dired-hide-details t)
(setq dired-dwim-target t)      ;;copys to the path of dired in the other window, very helpful for copying to/from server

(defun hpc ()
  "Opens the work folder no the HPC by SSH with dired."
  (interactive)
  (dired "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))

(defun double-commander ()
  "Opens two dired windows. Like Double commander"
  (interactive)
  (dired "/home/voit")
  (dired-other-window "/home/voit"))

(defun double-commander-remote ()
  "Opens two dired windows, on local, one on HPC. Like Double commander"
  (interactive)
  (dired "/home/voit")
  (dired-other-window "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))


(global-set-key (kbd "<f8>") 'double-commander)
(global-set-key (kbd "<f9>") 'double-commander-remote)

(add-hook 'dired-mode-hook
  (lambda ()
   (local-set-key [f5] 'dired-do-copy)
   (local-set-key [f6] 'dired-do-rename)
   (local-set-key (kbd "<tab>") 'other-window)
   (local-set-key (kbd "C-b") 'bookmark-jump)
   (local-set-key (kbd "C-<left>") 'dired-jump-other-window)
   (local-set-key (kbd "C-<right>") 'dired-jump-other-window)
   (local-set-key (kbd "<DEL>") 'dired-up-directory)))


;;========================================
;; Centaur Tabs
;;========================================
(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-c C-p")  'centaur-tabs-backward)
(global-set-key (kbd "C-c C-n") 'centaur-tabs-forward)
(setq centaur-tabs-set-icons t)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;(global-set-key (kbd "<M-o>") 'ace-window)

(when (display-graphic-p)
  (require 'all-the-icons))

;;========================================
;; Own functions
;;========================================

(defun init ()
  "Opens the init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun printhost ()
  "prints host name"
  (interactive)
  (message system-name))



