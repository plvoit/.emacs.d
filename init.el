;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
;;equire 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

;;(load "~/.emacs.d/elpa/dired-fixups/dired-fixups") dont know how to get this work
;;(require 'dired-fixups)
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
    dired-explorer  ;;jump to file starting with "keystroke"
    yafolding     ;;code folding
    reftex
    auctex
    shackle ;; set window sizes for major modes. E.g., for Python mode
    doom-modeline
    dashboard
    guru-mode
    popper
    company
    mini-frame
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
(menu-bar-mode -1)
(global-linum-mode t)               ;; Enable line numbers globally
(tool-bar-mode -1)                  ;; disable toolbar in GUI
(fset 'yes-or-no-p 'y-or-n-p)

;;(if (equal system-name "n-hpc-login1")
;;    (scroll-bar-mode -1))  ;; No visual indicator pleaser)
(if (display-graphic-p)
    (scroll-bar-mode -1))    

;; make tab key always call a indent command.
(setq-default tab-always-indent t)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;;show minibuffer on the top
;;(require 'mini-frame)
;;(mini-frame-mode 1) ;; Todo: doesnt work and disable company dictionary
(setq-default header-line-format mode-line-format) ; Copy mode-line
(setq-default mode-line-format nil) ; Remove mode-liney


(tooltip-mode nil)
(setq show-help-function nil)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;;Let's extend the standard C-x k with prefix support, so that we can invoke variations: Kill this buffer, kill other buffer, or kill all other buffers.
;;By default C-x k prompts to select which buffer should be selected. I almost always want to kill the current buffer, so let's not waste time making such
;;a tedious decision. Moreover, if I've killed a buffer, I usually also don't want the residual window, so let's get rid of it.

(global-set-key (kbd "C-x k")
  (lambda (&optional prefix)
"C-x k     ⇒ Kill current buffer & window
C-u C-x k ⇒ Kill OTHER window and its buffer
C-u C-u C-x C-k ⇒ Kill all other buffers and windows

Prompt only if there are unsaved changes."
     (interactive "P")
     (pcase (or (car prefix) 0)
       ;; C-x k     ⇒ Kill current buffer & window
       (0  (kill-this-buffer)
           (unless (one-window-p) (delete-window)))
       ;; C-u C-x k ⇒ Kill OTHER window and its buffer
       (4  (other-window 1)
           (kill-this-buffer)
           (unless (one-window-p) (delete-window)))
       ;; C-u C-u C-x C-k ⇒ Kill all other buffers and windows
       (16   (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
             (delete-other-windows)))))

(guru-global-mode +1)
;; ===================================
;; Dashboard
;; ===================================

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Dashboard")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
(setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package

(setq dashboard-set-file-icons t)
(setq dashboard-set-footer nil)


(require 'doom-modeline)
(doom-modeline-mode 1)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon nil)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon nil)

;; Whether display the colorful icon for `major-mode'.
;; It respects `nerdg-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon nil)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon nil)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; ===================================
;; Popper Windows
;; ===================================

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Python\\*"
	help-mode
        compilation-mode))

;; Match eshell, shell, term and/or vterm buffers
(setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                "^\\*term.*\\*$"   term-mode   ;term as a popup
                "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                )))

(global-set-key (kbd "C-o") 'popper-toggle-latest)  
(global-set-key (kbd "M-o") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)

;; For echo-area hints
(require 'popper-echo)
(popper-echo-mode +1)

;;C-m is the same as Enter, this changes it. From https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return

(if (equal system-name "GK-NB-14.ad.umwelt.uni-potsdam.de")
    (define-key input-decode-map [?\C-m] [C-m]))

;; Load the theme of choice:
(load-theme 'zenburn t)

(setq ido-enable-flex-matching t)    ;;ido mode settings
(setq ido-everywhere t)
(ido-mode 1)

;;(add-hook 'after-init-hook #'global-frame-mode)

;; Auto-complete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-mode-map (kbd "C-<tab>") 'company-complete)
;;(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

;;(add-to-list 'company-backends '(company-jedi)) ;;doesnt work


;; Start Emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '(default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(## auctex yafolding all-the-icons-dired all-the-icons nerd-icons centaur-tabs magit flycheck color-theme-sanityinc-tomorrow zenburn-theme color-theme multiple-cursors better-defaults))
 '(zoom-ignored-major-modes '(python-mode))
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
(global-set-key (kbd "<f10>") 'shell ) ;;open shell

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)


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

(desktop-save-mode 1)
;;===================================================
;;IDE Stuff
;;===================================================

(elpy-enable)

;;(add-hook 'elpy-mode-hook 'zoom-mode) ;;not very elegant. Does zoom mode stay switched off?
(add-hook 'elpy-mode-hook
          (lambda ()
            (zoom-mode -1)))

;;(require 'shackle)   ;; doesnt seem to work
;;(setq shackle-rules '(("*Python*" :align bottom :size 0.25)))

;; switch off auto documentation because it's annoying
(eldoc-mode -1)

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
(setq elpy-company-add-completion-from-shell t)
;;(setq elpy-shell-use-project-root nil)
(setq elpy-shell-display-buffer-after-send t)

;; always highlight matching parenthesis
(show-paren-mode 1)

(require 'multiple-cursors)   ;;multiple editing like in pycharm
(define-key global-map (kbd "M-j") 'mc/mark-next-like-this-word)

(require 'expand-region)           ;; smart expanding selection of expressions
(global-set-key (kbd "C-M-W") 'er/expand-region)


;;code folding based on indentation
(require 'yafolding)
(define-key global-map (kbd "C-*") 'yafolding-hide-all)
(define-key global-map (kbd "C-'") 'yafolding-show-all)




;;try to control postition and size of Python Shell
;; from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; places python shell at bottom, and small
(add-to-list 'display-buffer-alist
  '("*Python*" display-buffer-in-direction
    (direction . bottom)
    (window . root)
    (window-height . 0.3)))

;;this should place a fixed sidebar, but at least in Elpy this doesn't really work
;; left, top, right, bottom
;; (setq window-sides-slots '(0 0 1 0))

;; (add-to-list 'display-buffer-alist
;;           `("*Python*"
;;             display-buffer-in-side-window
;;             (side . right)
;;             (slot . 0)
;;             (window-parameters . ((no-delete-other-windows . t)))
;;             (window-width . 5)))


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

(setq dired-auto-revert-buffer 1) ;;update the file view
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))) ;;hide details

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
     ((equal xsortBy "ext") (setq xarg "-Al -X"))
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
   (local-set-key (kbd "<C-m>") 'dired-mark)  ;;somehow this special case needs to be in <>, because it conflicts with enter
   (local-set-key (kbd "C-g") 'revert-buffer)
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
;; Latex
;;========================================
;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook
      (lambda()
        (local-set-key [C-tab] 'TeX-complete-symbol)))
    
