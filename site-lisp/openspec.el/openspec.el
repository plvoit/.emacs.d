;;; openspec.el --- Interface for OpenSpec -*- lexical-binding: t; -*-

;; Copyright (C)

;; Author: Zacalot
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: ai, llm, openspec, agent
;; URL: https://github.com/Zacalot/openspec.el

;;; Commentary:

;; openspec.el provides an interface for managing OpenSpec
;; specifications and change proposals.  It wraps the `openspec' CLI tool
;; and presents its output in an interactive Emacs buffer.
;;
;; The package is split into the following modules:
;;   openspec-core.el     — CLI integration, project detection, utilities
;;   openspec-faces.el    — Face definitions
;;   openspec-status.el   — Status buffer mode and rendering
;;   openspec-commands.el — Interactive commands and transient menus
;;   openspec-conflict.el — Archive conflict resolution

;;; Usage:

;; Main entry point:
;;   M-x `openspec-status' to open the OpenSpec status buffer
;;
;; In the status buffer:
;;   g     Refresh
;;   RET   Open item at point
;;   TAB   Expand/collapse section
;;   c     Create new change
;;   v     Validate active change at point
;;   V     Validate section (bulk)
;;   a     Archive change at point (with confirmation)
;;   k     Delete change at point (with confirmation)
;;   w     Kill-Ring menu (apply command, name, etc.)
;;   i     Initialize project (if not initialized)
;;   n/p   Navigate between items and sections
;;   ?     Show help (transient menu)
;;   q     Quit
;;
;; Hooks for extensibility:
;;   `openspec-refresh-hook' - Run after refreshing the status buffer
;;   `openspec-archive-hook' - Run after successfully archiving a change

;;; Code:

;;; Customization

(defgroup openspec nil
  "Interface for OpenSpec."
  :group 'tools
  :prefix "openspec-")

(defcustom openspec-executable "openspec"
  "Path to the openspec executable."
  :type 'string
  :group 'openspec)

(defcustom openspec-status-key "C-c o"
  "Keybinding for `openspec-status'.
Set to nil to disable the global keybinding.
Use the `:set' handler via `customize-set-variable' or `setopt' to
apply the binding dynamically."
  :type '(choice (string :tag "Key sequence")
                 (const :tag "No keybinding" nil))
  :group 'openspec
  :set (lambda (sym val)
         (when (boundp 'openspec-status-key)
           (let ((old-key (symbol-value sym)))
             (when old-key
               (global-unset-key (kbd old-key)))))
         (set-default sym val)
         (when val
           (global-set-key (kbd val) #'openspec-status))))

(defcustom openspec-minimum-version "1.1.0"
  "Minimum required version of the openspec CLI."
  :type 'string
  :group 'openspec)

;;; Sub-modules

;; Ensure the directory containing openspec.el is on the load-path so
;; that sub-module files (openspec-core.el, etc.) can be found.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir
    (add-to-list 'load-path dir)))

(require 'openspec-faces)
(require 'openspec-core)
(require 'openspec-status)
(require 'openspec-commands)
(require 'openspec-conflict)

;;; Entry Point

;;;###autoload
(defun openspec-status ()
  "Open the OpenSpec status buffer for the current project."
  (interactive)
  (unless (openspec--cli-available-p)
    (user-error "OpenSpec CLI not found. Please install it from https://openspec.dev"))
  (unless (openspec--version-ok-p)
    (user-error "OpenSpec CLI version %s is too old (minimum: %s). Please upgrade"
                (or (openspec--version) "unknown") openspec-minimum-version))
  (let ((root (openspec--project-root)))
    (unless root
      (user-error "Could not determine project root"))
    (let ((buf (get-buffer-create (openspec--buffer-name root))))
      (with-current-buffer buf
        (unless (eq major-mode 'openspec-mode)
          (openspec-mode))
        (setq openspec--project-root root)
        (setq default-directory root)
        (openspec-refresh))
      (pop-to-buffer buf))))

;;; Startup Dashboard

(defun openspec--open-dashboard-for-dired (dired-buf)
  "Open OpenSpec dashboard to the right of the window showing DIRED-BUF.
Only acts when DIRED-BUF is displayed, its directory is a project root,
and no dashboard for that project is already visible."
  (when (buffer-live-p dired-buf)
    (when-let* ((win (get-buffer-window dired-buf t)))
      (with-current-buffer dired-buf
        (when-let* ((root (openspec--find-root default-directory))
                    (_ (file-equal-p (file-truename default-directory)
                                     (file-truename root)))
                    (_ (not (get-buffer-window (openspec--buffer-name root) t))))
          (let* ((spec-buf (get-buffer-create (openspec--buffer-name root)))
                 (right-win (split-window win nil 'right)))
            (with-current-buffer spec-buf
              (unless (eq major-mode 'openspec-mode)
                (openspec-mode))
              (setq openspec--project-root root)
              (setq default-directory root)
              (openspec-refresh))
            (set-window-buffer right-win spec-buf)
            (select-window win)))))))

(defun openspec--maybe-open-dashboard ()
  "Schedule a dashboard check after dired is shown in a window.
Only schedules when the current directory directly contains an openspec/ dir."
  (when (file-directory-p (expand-file-name "openspec" default-directory))
    (run-with-idle-timer 0 nil #'openspec--open-dashboard-for-dired (current-buffer))))

(add-hook 'dired-mode-hook #'openspec--maybe-open-dashboard)

(provide 'openspec)

;;; openspec.el ends here
