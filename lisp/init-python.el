;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
;; Install: pip install pyflakes autopep8
;; for this to work you need to install the python-lsp packages via pip:
;; $ pip install "python-lsp-server[all]"
;; $  pip install pyls
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))



;;===============================
;; Python Settings
;;===============================

(if (equal system-name "n-hpc-login1")
    (setenv "WORKON_HOME" "/home/voit/.conda/envs/")
    (setenv "WORKON_HOME" "/home/voit/miniconda3/envs/"))


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


;; (defun python-send-and-step ()
;;   "Send the current line to the Python shell and move to the next line."
;;   (interactive)
;;   (python-shell-send-statement (thing-at-point 'line))
;;   (next-line))


;;try to control postition and size of Python Shell
;; from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; places python shell at bottom, and small
;;(add-hook 'elpy-mode-hook 'zoom-mode) ;;not very elegant. Does zoom mode stay switched off?
(add-hook 'python-mode-hook
          (lambda ()
            (zoom-mode -1)))


;; (defun elpy-shell-send-statement-and-step ()
;;   "Send current or next statement to Python shell and step.

;; If the current line is part of a statement, sends this statement.
;; Otherwise, skips forward to the next code line and sends the
;; corresponding statement."
;;   (interactive)
;;   (elpy-shell--ensure-shell-running)
;;   (elpy-shell--nav-beginning-of-statement)
;;   ;; Make sure there is a statement to send
;;   (unless (looking-at "[[:space:]]*$")
;;     (unless elpy-shell-echo-input (elpy-shell--append-to-shell-output "\n"))
;;     (let ((beg (save-excursion (beginning-of-line) (point)))
;;           (end (progn (elpy-shell--nav-end-of-statement) (point))))
;;       (unless (eq beg end)
;;         (elpy-shell--flash-and-message-region beg end)
;;         (elpy-shell--add-to-shell-history (buffer-substring beg end))
;;         (elpy-shell--with-maybe-echo
;;          (python-shell-send-string
;;           (python-shell-buffer-substring beg end)))))
;;     (python-nav-forward-statement)))


;;this works
(defun python-send-and-step ()
  "Send the current line to the Python shell and move to the next line."
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning) (region-end))
      (python-shell-send-statement (thing-at-point 'line)))
  (next-line))

;; this works
(defun python-send-statment ()
  "Sends a statement to shell"
  (interactive)
    (unless (looking-at "[[:space:]]*$")
     (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (progn (python-nav-end-of-block) (point))))
          (python-shell-buffer-substring beg end)))
  (python-nav-forward-statement)
  )

(defun python-send-and-step-smart ()
  "Send the current line to the Python shell and move to the next line."
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning) (region-end))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (progn (python-nav-forward-sexp) (point))))
      (message "The beg is: %s" beg)
      (message "The end is: %s" end)
      (message "Substring: %s" (buffer-substring beg end)
      (unless (eq beg end)
        (python-shell-send-string (python-shell-buffer-substring beg end))
        (next-line))
      (python-shell-send-statement (thing-at-point 'line))
      (next-line)))))

;; (defun my-run-python ()
;;   "Opens Python shell without switching to its window"
;;   (interactive)
;;   (run-python))
;;  ;; (other-window))

(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key (kbd "C-r") 'python-send-and-step-smart)))


(global-set-key (kbd "<f5>") 'realgud:pdb)

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
