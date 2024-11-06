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

;;set auto format wider
(setq blacken-line-length 120)

(if (equal system-name "n-hpc-login1")
    (setenv "WORKON_HOME" "/home/voit/.conda/envs/")
    (setenv "WORKON_HOME" "/home/voit/miniconda3/envs/"))


;;code folding based on indentation
(require 'yafolding)
(define-key global-map (kbd "C-c f") 'hs-hide-all)
(define-key global-map (kbd "C-c u") 'hs-show-all)


(if (display-graphic-p)
    (scroll-bar-mode -1))    




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

;; This function almost does the job. The only problem is, that if two lines are not separated
;; by a blank line, then it sends both lines
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

(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key (kbd "C-r") 'python-send-and-step-smart)
    (local-set-key (kbd "<tab>") 'python-indent-shift-right)
    (local-set-key (kbd "<backtab>") 'python-indent-shift-left)
    (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)))
;;enable code folding
(add-hook 'python-mode-hook #'hs-minor-mode)

(global-set-key (kbd "<f5>") 'realgud:pdb)

(add-hook 'python-mode-hook (lambda () (eldoc-mode -1))) ;;hide documentation
(add-hook 'python-mode-hook (lambda () (company-mode -1)))
(add-hook 'python-mode-hook (lambda () (lsp-headerline-breadcrumb-mode -1))) ;;something is switching it on but the corfu and company run at the same time

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
