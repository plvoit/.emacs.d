;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
;;equire 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

(load "~/.emacs.d/elpa/dired-fixups/dired-fixups")
(require 'dired-fixups)
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
    color-theme-sanityinc-tomorrow
    flycheck                    ;; syntax check for python, seems to work less buggy than flymake
    magit
    all-the-icons              ;;all-the-icons-install-fonts might be needed for the first run (out of emacs)
    all-the-icons-dired
    dired-explorer
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


;;C-m is the same as Enter, this changes it. From https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
(define-key input-decode-map [?\C-m] [C-m])

;; Load the theme of choice:
(load-theme 'zenburn t)

(setq ido-enable-flex-matching t)       ;;ido mode settings
(setq ido-everywhere t)
(ido-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Start Emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '(default))
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

(defun day ()
  "Switches to day theme"
  (interactive)
  (load-theme 'sanityinc-tomorrow-day t))

;; tramp tries to compress remote and decompress local. THis messes up python files. Change limits here
(setq tramp-inline-compress-start-size "20000000") ;;20Mb
(setq tramp-copy-size-limit "20000000000");; 20GB

(global-set-key (kbd "C-j") 'set-rectangular-region-anchor) ;;multiple cursors
;;===================================================
;;Tab bars. Taken from https://amitp.blogspot.com/2020/06/emacs-prettier-tab-line.html
;;===================================================
(global-tab-line-mode t)

(require 'powerline)
(defvar my/tab-height 22)
(defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
(defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list my/tab-left
                          (format " %s  " (buffer-name buffer))
                          my/tab-right)))
(setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)

(set-face-attribute 'tab-line-tab nil ;; active tab in another window
      :inherit 'tab-line
      :foreground "gray70" :background "gray90" :box nil)
(set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
      :background "#ffd699" :foreground "black" :box nil)

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

(global-set-key (kbd "<f2>") 'menu-bar-open)


;;===============================================
;; Folder and Double commander settings
;;===============================================
(setq dired-hide-details t)
(setq dired-dwim-target t)      ;;copys to the path of dired in the other window, very helpful for copying to/from server
;;(setq dired-kill-when-opening-new-dired-buffer nil)

;;show file size in Megabyte
(setq dired-listing-switches "-lhaG")

(require 'dired-explorer)
(add-hook 'dired-mode-hook 'dired-explorer-mode)

;; sorting function from http://xahlee.info/emacs/emacs/dired_sort.html
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (let (xsortBy xarg)
    (setq xsortBy (completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal xsortBy "name") (setq xarg "-Al "))
     ((equal xsortBy "date") (setq xarg "-Al -t"))
     ((equal xsortBy "size") (setq xarg "-Al -S"))
     ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other xarg )))


(put 'dired-find-alternate-file 'disabled nil)

;; from https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;; prevent updir to create a new buffer
(eval-after-load "dired"
 ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
    "Run Dired on parent directory of current directory."
       (interactive "P")
       (let* ((dir (dired-current-directory))
       (orig (current-buffer))
       (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
       ;; Only try dired-goto-subdir if buffer has more than one dir.
       (and (cdr dired-subdir-alist)
       (dired-goto-subdir up))
       (progn
     	  (kill-buffer orig)
     	  (dired up)
     	  (dired-goto-file dir))))))

(defun hpc ()
  "Opens the work folder no the HPC by SSH with dired."
  (interactive)
  (dired "/ssh:voit@login/work/voit"))


(defun double-commander ()
 "Opens two dired windows. Like Double Commander."
 (interactive)
 (if (equal system-name "n-hpc-login1")
     (progn
       (dired "/work/voit")
       (dired-other-window "/work/voit"))
     (progn
       (dired "/home/voit")
       (dired-other-window "/home/voit"))))


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
   (local-set-key [f7] 'dired-create-directory)
   (local-set-key (kbd "<tab>") 'other-window)
   (local-set-key (kbd "C-b") 'bookmark-jump)
   (local-set-key (kbd "<DEL>") 'dired-up-directory)
   (local-set-key (kbd "C-o") 'dired-find-file-other-window)
   (local-set-key (kbd "C-d") 'dired-do-delete)
   (local-set-key (kbd "<C-m>") 'dired-mark)
   (local-set-key (kbd "C-M-s") 'xah-dired-sort)))
   ;;(local-set-key (kbd "<RET>") 'dired-find-file)))
   ;;(local-set-key (kbd "<RET>") 'dired-find-alternate-file))) ;; this command closes the buffer in the other window.... 
   ;;(local-set-key (kbd "<DEL>") 'dired-find-alternate-file "..")))
   


;;========================================
;; Centaur Tabs
;;========================================
;; (require 'centaur-tabs)
;; (centaur-tabs-mode t)
;; (global-set-key (kbd "C-c C-p")  'centaur-tabs-backward)
;; (global-set-key (kbd "C-c C-n") 'centaur-tabs-forward)
;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-modified-marker "⬤")


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



;;========================================
;; Dired fixups
;;========================================

