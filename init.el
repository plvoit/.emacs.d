;; .emacs.d/init.el

;; ===================================
;; straight.el bootstrap
;; ===================================
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; ===================================
;; Package setup (package.el for existing elpa installs)
;; ===================================
(require 'package)
(package-initialize)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))


(update-load-path)

(eval-and-compile
  (setq use-package-expand-minimally t))


;; ===================================
;; Basic Customization
;; ===================================
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb


;; Garbage Collector Magic Hack
(use-package gcmh
  :straight t
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ;; 16MB    

;;=======================================
;; Packages (straight.el managed)
;;=======================================
(use-package better-defaults :straight t :demand t :config (ido-mode -1))
(use-package zoom :straight t :if (display-graphic-p) :hook (after-init . zoom-mode))
(use-package markdown-mode :straight t :defer t)
(use-package magit :straight t :defer t)
(use-package flycheck :straight t :defer t)
(use-package yafolding :straight t :defer t :hook (prog-mode . yafolding-mode))
(use-package shackle :straight t)
(use-package auctex :straight t :defer t)
(use-package zenburn-theme :straight t :defer t)
(use-package color-theme-sanityinc-tomorrow :straight t :defer t)
(use-package pyvenv :straight t :defer t)
(use-package dired-explorer :straight t :hook (dired-mode . dired-explorer-mode))

;;=======================================
;; eat terminal emulator (via straight.el)
;;=======================================
(use-package eat
  :straight (:host codeberg :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eshell-load . eat-eshell-mode))

;;Terminal specific settings
(add-hook 'term-setup-hook
  (lambda ()
    (define-key function-key-map "\e[1;5A" [up])
    (define-key function-key-map "\e[1;5B" [down])
    (define-key function-key-map "\e[1;5C" [right])
    (define-key function-key-map "\e[1;5D" [left])
    (global-set-key (kbd "M-*") 'hs-show-all)
    (global-set-key (kbd "M-'") 'hs-hide-all)))



(use-package which-key
  :straight t
  :defer t
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-bottom))


(setq inhibit-startup-message t)    ;; Hide the startup message
(menu-bar-mode -1)
;;(global-linum-mode 0)               ;; Disable line numbers globally

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
(setq make-backup-files nil)

(setq doc-view-continuous 1)

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
       (0  (kill-buffer (current-buffer))
           (unless (one-window-p) (delete-window)))
       ;; C-u C-x k ⇒ Kill OTHER window and its buffer
       (4  (other-window 1)
           (kill-buffer (current-buffer))
           (unless (one-window-p) (delete-window)))
       ;; C-u C-u C-x C-k ⇒ Kill all other buffers and windows
       (16   (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
             (delete-other-windows)))))

;; (use-package guru-mode
;;   :straight t
;;   :config (guru-global-mode +1))

;;ediff set vertical split as default-directory
;; (custom-set-variables
;;  '(ediff-window-setup-function 'ediff-setup-windows-plain)
;;  '(ediff-diff-options "-w")
;;  '(ediff-split-window-function 'split-window-horizontally))

;;==================================
;; Editing
;;==================================

(use-package multiple-cursors
  :straight t
  :defer t
  :bind ("M-j" . mc/mark-next-like-this-word))

(use-package expand-region
  :straight t
  :defer t
  :bind ("C-M-W" . er/expand-region))


;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c l") 'goto-line)

;; ===================================
;; Dashboard
;; ===================================

(use-package dashboard
  :straight t
  :demand t
  :config (dashboard-setup-startup-hook))
;; Set the title
(setq dashboard-banner-logo-title "Use registers! \nC-x r Space = set register, C-x r j = jump to register \nC-x r s = save region to registers, C-x r i = insert region\nrgrep find text in files\nfind-name-dired for wildcard file search\nC-j = multiple cursors\nC-x 0 = delete window\nC-x C-j = dired jump to folder of current buffer, C-M-n = copy filename, C-M-p = copy filename with fullpath\nC-M n(or p) = navigate parenthesis\nM-y = cycle through kills, C-x C-x = jump point to last position?\nC-c m = toggle markdown rendered preview / raw text\nC-c c = start Claude")
;; Set the banner
(setq dashboard-startup-banner (if (display-graphic-p) 'logo 1))
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)))

;; ===================================
;; Modeline
;; ===================================

(use-package mood-line
  :straight t
  :config (mood-line-mode))


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
                )))

(with-eval-after-load 'vterm
  (add-to-list 'vterm-keymap-exceptions "C-o"))


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

(use-package popper
  :straight t
  :bind (("C-o"   . popper-toggle-latest)
         ("M-o"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (popper-mode +1)
  (use-package popper-echo
    :config (popper-echo-mode +1)))

;;C-m is the same as Enter, this changes it. From https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
;;(if (equal system-name "GK-NB-14.ad.umwelt.uni-potsdam.de")
;;    (define-key input-decode-map [?\C-m] [C-m]))

; (when (display-graphic-p)
;   (define-key input-decode-map [?\C-m] ["RET"]))

;;(define-key input-decode-map [C-m] ["RET"])    

(use-package catppuccin-theme :straight t)

;; Load the theme of choice:
(setq catppuccin-flavor 'frappe) ;; options: latte frappe macchiato mocha
(load-theme 'catppuccin :no-confirm)

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


;; Start Emacs in fullscreen mode (GUI only; terminal fills the terminal window)
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


;;===================================================
;;Custom functions
;;===================================================


(defun day ()
  "Switches to Catppuccin Latte (light theme)"
  (interactive)
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin :no-confirm)
  (catppuccin-reload))


(defun night ()
  "Switches to Catppuccin Frappé (cool dark theme)"
  (interactive)
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm)
  (catppuccin-reload))

(defun conda (environment)
  "Loads a Python conda environment."
  (interactive "sEnter the conda environment name: ")
  (pyvenv-workon environment))


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

(if (display-graphic-p)
    (progn
      (use-package powerline :straight t)
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
                          :foreground "#CC9393" :box nil))
  ;; Terminal: plain tab-line without powerline bitmap separators
  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  (set-face-attribute 'tab-line-tab-current nil
                      :background "#e5c890" :foreground "black" :bold t :box nil))

;; desktop-save-mode disabled: re-spawns LSP servers for all previous files on startup
;; Use consult-recent-file (C-c r) to re-open what you need
;;(desktop-save-mode 1)

;;===================================================
;; OpenSpec agent buffer naming
;;===================================================
(defun my/openspec-agent-rename-on-opsx ()
  "Rename agent buffer when an opsx command is submitted.
Reads input before shell-maker-submit clears it, then defers the
rename via a zero-second timer so shell-maker-submit can still look
up the buffer by its original name."
  (when (derived-mode-p 'agent-shell-mode)
    (let* ((proc (get-buffer-process (current-buffer)))
           (input (when proc
                    (string-trim
                     (buffer-substring-no-properties
                      (process-mark proc) (point-max)))))
           (buf (current-buffer)))
      (when (and input (string-prefix-p "/opsx:" input))
        (cond
         ((string-match "^/opsx:\\(?:apply\\|propose\\|explore\\)[ \t]+\\(\\S-+\\)" input)
          (let ((change (match-string 1 input)))
            (run-with-timer
             0 nil
             (lambda (b name)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unless (local-variable-p 'my/openspec-original-buffer-name)
                     (setq-local my/openspec-original-buffer-name (buffer-name)))
                   (rename-buffer (format "c@%s" name) t)
                   (setq-local shell-maker--buffer-name-override (buffer-name)))))
             buf change)))
         ((string-match "^/opsx:archive" input)
          (run-with-timer
           0 nil
           (lambda (b)
             (when (buffer-live-p b)
               (with-current-buffer b
                 (when (local-variable-p 'my/openspec-original-buffer-name)
                   (rename-buffer my/openspec-original-buffer-name t)
                   (setq-local shell-maker--buffer-name-override (buffer-name))
                   (kill-local-variable 'my/openspec-original-buffer-name)))))
           buf)))))))

(advice-add 'shell-maker-submit :before #'my/openspec-agent-rename-on-opsx)

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
(setq dired-kill-when-opening-new-dired-buffer 1)

(setq dired-auto-revert-buffer 1) ;;update the file view
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))) ;;hide details

;;show file size in Megabyte
(setq dired-listing-switches "-lhaG --group-directories-first")

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

(defun hpc ()
  "Opens the work folder no the HPC by SSH with dired."
  (interactive)
  (dired "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))


(defun my-dired-fullpath-filename ()
  "Copy filename and full path"
  (interactive)
  (let ((current-prefix-arg 0))
    (call-interactively 'dired-copy-filename-as-kill)))


(add-hook 'dired-mode-hook
  (lambda ()
   (local-set-key [f5] 'dired-do-copy)
   (local-set-key [f6] 'dired-do-rename)
   (local-set-key [f7] 'dired-create-directory)
   (local-set-key (kbd "<tab>") 'other-window)
   (local-set-key (kbd "C-b") 'bookmark-jump)
   ;;(local-set-key (kbd "<DEL>") 'dired-up-directory) ;; a bit stupid that for dired-sidebar its "-", oterwise there is a weird conflict
   ;;(local-set-key (kbd "-") 'dired-up-directory)
   (local-set-key (kbd "C-o") 'dired-find-file-other-window)
   (local-set-key (kbd "C-d") 'dired-do-delete)
   (local-set-key (kbd "<C-m>") 'dired-mark)  ;;somehow this special case needs to be in <>, because it conflicts with enter
   (local-set-key (kbd "C-g") 'revert-buffer)
   (local-set-key (kbd "C-M-s") 'xah-dired-sort)
   (local-set-key (kbd "C-M-n") 'dired-copy-filename-as-kill)
   (local-set-key (kbd "C-M-p") 'my-dired-fullpath-filename)))
   ;;(local-set-key (kbd "<RET>") 'dired-find-file)))
   ;;(local-set-key (kbd "<RET>") 'dired-find-alternate-file))) ;; this command closes the buffer in the other window.... 
   ;;(local-set-key (kbd "<DEL>") 'dired-find-alternate-file "..")))
   

;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;(when (display-graphic-p)
;;  (require 'all-the-icons))
;; doesnt work in terminal mode
;; (require 'nerd-icons)
;; (require 'nerd-icons-dired)
;; (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
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
  :straight t
  :custom
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-prefix 2)             ;; Trigger auto completion with 2 chars
  (corfu-quit-at-boundary t)        ;; Automatically quit at word boundary
  (corfu-quit-no-match t)           ;; Automatically quit if there is no match
  (corfu-preview-current nil)       ;; Disable current candidate preview
  (corfu-preselect 'prompt)         ;; Preselect the prompt
  (corfu-scroll-margin 5)           ;; Use scroll margin
  (global-corfu-minibuffer nil)     ;; Let vertico handle the minibuffer
  :bind ("<backtab>" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Add extensions
(use-package cape
  :straight t
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
(require 'init-double-commander)
(use-package openspec
  :load-path "site-lisp/openspec.el")



;;=======================================
;; agent-shell (LLM agent via ACP protocol)
;;=======================================
(use-package shell-maker :straight t)

(with-eval-after-load 'shell-maker
  (defvar shell-maker--curl-version-supported-cache 'unchecked)
  (advice-add 'shell-maker--curl-version-supported :override
              (lambda ()
                (when (eq shell-maker--curl-version-supported-cache 'unchecked)
                  (setq shell-maker--curl-version-supported-cache
                        (let ((str (shell-command-to-string
                                    (concat shell-maker-curl-executable " --version "))))
                          (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" str)
                            (version<= "7.76" (match-string 1 str))))))
                shell-maker--curl-version-supported-cache)))

(use-package acp :straight t)
(use-package agent-shell
  :straight t
  :after (shell-maker acp))

(global-set-key (kbd "C-c c") 'agent-shell-anthropic-start-claude-code)

(with-eval-after-load 'agent-shell-diff
  (require 'agent-shell-diff-edit))

(setq markdown-command "pandoc")

(defvar my/markdown-skip-preview nil
  "When non-nil, markdown-mode-hook skips opening the rendered preview.")

(defvar-local my/markdown-source-file nil
  "In an eww preview buffer, the path of the originating markdown file.")

(defun my/markdown-open-as-preview ()
  "Render current markdown file with pandoc and display in eww. Kill the source buffer."
  (unless my/markdown-skip-preview
    (let* ((md-buf (current-buffer))
           (md-file (buffer-file-name))
           (tmp-html (make-temp-file "md-preview-" nil ".html")))
      (when md-file
        (call-process "pandoc" nil nil nil
                      "-f" "markdown" "-t" "html5" "--standalone"
                      md-file "-o" tmp-html)
        (let ((display-buffer-overriding-action '(display-buffer-same-window)))
          (eww-open-file tmp-html))
        (setq-local my/markdown-source-file md-file)
        (run-with-timer 0 nil
          (lambda ()
            (when (buffer-live-p md-buf)
              (kill-buffer md-buf))))))))

(defun my/markdown-toggle ()
  "Toggle between rendered markdown preview (eww) and raw markdown source."
  (interactive)
  (cond
   ((and (derived-mode-p 'eww-mode) my/markdown-source-file)
    (let ((md-file my/markdown-source-file)
          (eww-buf (current-buffer)))
      (let ((my/markdown-skip-preview t))
        (find-file md-file))
      (kill-buffer eww-buf)))
   ((derived-mode-p 'markdown-mode)
    (my/markdown-open-as-preview))
   (t
    (message "Not in a markdown preview or source buffer"))))

(add-hook 'markdown-mode-hook 'my/markdown-open-as-preview)
(global-set-key (kbd "C-c m") 'my/markdown-toggle)

;;=======================================
;;dired sidebar
;;=======================================

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar))
(with-eval-after-load 'dired-sidebar
  ;; Unbind "^" in `dired-sidebar-mode-map`
  (define-key dired-sidebar-mode-map (kbd "^") nil))

;;(global-set-key (kbd "^") 'dired-sidebar-toggle-sidebar)
(defun my/dired-sidebar-toggle-and-jump ()
  "Toggle the dired-sidebar and jump to it if it's open."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (when (bound-and-true-p dired-sidebar-mode)
    (dired-sidebar-jump-to-sidebar)))

(global-set-key (kbd "^") 'my/dired-sidebar-toggle-and-jump)

(setq dired-sidebar-theme 'none)

;; Enable jump-to-letter functionality for all printable keys in dired-sidebar
;; this works just if we have no Icons for dired
(defun my/dired-sidebar-jump-to-letter ()
  "Jump to the first file or directory in dired-sidebar starting with the letter pressed.
Ignores capitalization."
  (interactive)
  (let* ((char (read-char "Jump to: ")) ;; Read a single character
         (case-fold-search t)          ;; Ignore case
         (search-regexp (format "^\\s-*%c" (upcase char)))) ;; Build the search regex
    (goto-char (point-min)) ;; Start from the beginning
    (unless (re-search-forward search-regexp nil t)
      (message "No file or directory found starting with '%c'" char))))
;; Hook it into dired-sidebar mode
(with-eval-after-load 'dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'my/dired-sidebar-activate-jump-to-letter))
;;=======================================
;;Neotree sidebar
;;=======================================
;; Bind the function to Alt-1
;;(global-set-key (kbd "M-1") 'neotree-toggle)
;;(with-eval-after-load 'neotree
;;  (define-key neotree-mode-map (kbd "<backspace>") 'neotree-select-up-node))
;;

;;=======================================
;;treemacs sidebar
;;=======================================

;;(use-package treemacs
;;  :ensure t
;;  :defer t
;;  :config
;;  (setq treemacs-collapse-dirs              (if treemacs-python-executable 3 0)
;;        treemacs-follow-after-init          t
;;        treemacs-file-follow-delay          0.2)
;;  :bind
;;  (:map global-map
;;        ("M-0"       . treemacs-select-window)
;;        ("C-x t t"   . treemacs)
;;        ("C-x t 1"   . treemacs-delete-other-windows)))
;;=================================================
;;Notes
;;=================================================

;;rgrep
;; vertico mode will always complete the expression with the first suggestion
;; to proceed without completion, press M+Enter (Ret).This way one can search with wildcards
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-preselect 'first)
 '(lsp-headerline-breadcrumb-enable nil)
 '(package-selected-packages
   '(gptel dired-sidebar csv-mode poetry consult-eglot consult-lsp quelpa realgud pyvenv powerline lsp-pyright lsp-mode eglot which-key doom-themes gcmh embark-consult embark consult-flyspell consult vertico-posframe orderless vertico marginalia use-package cape corfu mini-frame company popper guru-mode dashboard doom-modeline auctex yafolding dired-explorer all-the-icons-dired shackle all-the-icons magit flycheck color-theme-sanityinc-tomorrow zenburn-theme expand-region multiple-cursors markdown-mode zoom better-defaults corfu-terminal)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:extend t :background "gray")))))


