;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;;
;; (unless (or (daemonp) noninteractive init-file-debug)
;;   ;; Suppress file handlers operations at startup
;;   ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
;;   (let ((old-value file-name-handler-alist))
2;;     (setq file-name-handler-alist nil)
;;     (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
;;    (add-hook 'emacs-startup-hook
;;              (lambda ()
;;                "Recover file name handlers."
;;                (setq file-name-handler-alist
;;                      (delete-dups (append file-name-handler-alist old-value))))
;;              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)



(update-load-path)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initializes the package infrastructure
(package-initialize)


(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;(package-refresh-contents) ;; needed if packages are sometimes not installed properly

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    ;;elpy                            ;; Python IDE
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
    shackle
    all-the-icons-dired
    dired-explorer  ;;jump to file starting with "keystroke"
    yafolding     ;;code folding
    reftex
    auctex
    doom-modeline
    dashboard
    guru-mode
    popper
    company
    mini-frame
    corfu
    cape
    use-package
    emacs
    marginalia
    vertico
    orderless
    vertico-posframe
    consult
    consult-flyspell
    embark
    embark-consult
    gcmh
    doom-themes
    which-key
    eglot
    lsp-mode
    lsp-pyright
    powerline
    pyvenv
    realgud
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
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb


;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ;; 16MB    


(add-hook 'term-setup-hook
  (lambda ()
    (define-key function-key-map "\e[1;5A" [up])
    (define-key function-key-map "\e[1;5B" [down])
    (define-key function-key-map "\e[1;5C" [right])
    (define-key function-key-map "\e[1;5D" [left])))

;;Enable key help functions
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)


(setq inhibit-startup-message t)    ;; Hide the startup message
(menu-bar-mode -1)
(global-linum-mode 0)               ;; Disable line numbers globally

;; also in lisp mode
(add-hook  'emacs-lisp-mode-hook #'(lambda()(display-line-numbers-mode -1)))


(tool-bar-mode -1)                  ;; disable toolbar in GUI
(fset 'yes-or-no-p 'y-or-n-p)
;;(setq ring-bell-function 'ignore)
(setq visible-bell t)
;;(if (equal system-name "n-hpc-login1")
;;    (scroll-bar-mode -1))  ;; No visual indicator please) 
(if (display-graphic-p)
    (scroll-bar-mode -1))    

;; make tab key always call a indent command.
;;(setq-default tab-always-indent t)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
(setq delete-by-moving-to-trash t)

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


;;==================================
;; Editing
;;==================================

(require 'multiple-cursors)   ;;multiple editing like in pycharm
(define-key global-map (kbd "M-j") 'mc/mark-next-like-this-word)

(require 'expand-region)           ;; smart expanding selection of expressions
(global-set-key (kbd "C-M-W") 'er/expand-region)


;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; ===================================
;; Dashboard
;; ===================================

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Use registers! \nC-x r Space = set register, C-x r j = jump to register \nC-x r s = save region to registers, C-x r i = insert region\nrgrep find text in files\nfind-name-dired for wildcard file search\nC-j = multiple cursors\nC-x 0 = delete window")
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

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)))

;; ===================================
;; Doom Modeline
;; ===================================


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


;;show minibuffer on the top
;;(require 'mini-frame)
;;(mini-frame-mode 1) ;; Todo: disable company dictionary
;;(setq-default header-line-format mode-line-format) ; Copy mode-line this seems to work but maybe the info and help can now not be seen anymore
;;(setq-default mode-line-format nil) ; Remove mode-liney


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


;; (defun popper-toggle-latest-modified (&optional arg)									
;;   "Toggle visibility of the last opened popup 
;; With prefix ARG \\[universal-argument], toggle visibility of the next popup windows	  
;; while keeping the current one (FIXME: This 
;; With a double prefix ARG \\[universal-argument]										  
;; \\[universal-argument], toggle all popup-windows. Note that only						 
;; one buffer can be show in one slot, so it will display as many 
;; windows as it can."											   
;;   (interactive "p")											   
;;   (let ((group (when popper-group-function					   
;;                  (funcall popper-group-function))))			   
;;     (if popper-open-popup-alist								   
;;         (pcase arg											   
;;           (4 (popper-open-latest group))						   
;;           (16 (popper--bury-all))							   
;;           (_ (popper-close-latest)))							   
;;       (if (equal arg 16)										   
;;           (popper--open-all)									   
;;         (popper-open-latest group)))							   
;;     (end-of-buffer-other-window)								   
;; 	))															   

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

;;(setq ido-enable-flex-matching t)    ;;ido mode settings
;;(setq ido-everywhere t)
;;(ido-mode 1)

;;(add-hook 'after-init-hook #'global-frame-mode)

;; Auto-complete
;;(require 'company)
;;(add-hook 'after-init-hook 'global-company-mode)
;;(define-key company-mode-map (kbd "C-<tab>") 'company-complete)
;;(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

;;(add-to-list 'company-backends '(company-jedi)) ;;doesnt work


;; Start Emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(realgud-python realgud consult-eglot consult-lsp csv-mode doom-themes use-package corfu cape better-defaults))
 '(zoom-ignored-major-modes '(python-mode))
 '(zoom-mode t nil (zoom)))

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;===================================================
;;Custom functions
;;===================================================


(defun day ()
  "Switches to day theme"
  (interactive)
  (load-theme 'sanityinc-tomorrow-day t))

(defun dark ()
  "Switches to dark theme"
  (interactive)
  (load-theme 'zenburn t))

(defun conda ()
  "Loads a Python conda environment"
  (interactive)
  (pyvenv-workon))



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
(set-face-attribute 'tab-line-tab-modified nil ;; modified tab in current window
		    :background "#3B3B3B" :foreground "#ffd699" :box nil)

(desktop-save-mode 1)

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
(setq dired-listing-switches "-lhaG --group-directories-first")

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
   

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

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


;;=======================================
;;Corfu auto complete
;;=======================================
(use-package corfu
  :custom
  (corfu-auto t)                 ;; Disable auto completion
  (corfu-auto-prefix 2)          ;; Trigger auto completion with 2 chars
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

;;this messes with the colors in terminal mode. Comes from init-corfu from centaur emacs
;; but I deactivated it
;;(unless (display-graphic-p)
;;  (use-package corfu-terminal

;;:hook (global-corfu-mode . corfu-terminal-mode)))

;; Add extensions
(use-package cape
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))  


(require 'init-completion)
(require 'init-lsp)
(require 'init-python)
;;=================================================
;;Notes
;;=================================================

;;rgrep
;; vertico mode will always complete the expression with the first suggestion
;; to proceed without completion, press M+Enter (Ret).This way one can search with wildcards
