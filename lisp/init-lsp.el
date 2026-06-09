;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2023 Vincent Zhang

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
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((yaml-mode yaml-ts-mode) . eglot-ensure))
  :config
  (use-package consult-eglot
    :straight t
    :bind (:map eglot-mode-map
            ("C-M-." . consult-eglot-symbols))))


(setq read-process-output-max (* 1024 1024)) ;; 1MB for LSP

;;======================================================   
;; Debug
;;======================================================

(use-package realgud
  :straight t
  :defer t
  :commands (realgud:pdb realgud:ipdb realgud:gdb)
  :config (setq realgud-safe-mode nil))


;; (require 'dap-python)
;; ;; ;; if you installed debugpy, you need to set this
;; ;; ;; https://github.com/emacs-lsp/dap-mode/issues/306
;; (setq dap-python-debugger 'debugpy)


;; (use-package dap-mode
;;   :disabled
;;   :defines dap-python-executable
;;   :functions dap-hydra/nil
;;   :diminish
;;   :bind (:map lsp-mode-map
;;          ("<f5>" . dap-debug)
;;          ("M-<f5>" . dap-hydra))
;;   :hook ((after-init     . dap-auto-configure-mode)
;;          (dap-stopped    . (lambda (_) (dap-hydra)))
;;          (dap-terminated . (lambda (_) (dap-hydra/nil)))

;;          ((python-mode python-ts-mode)            . (lambda () (require 'dap-python)))
;; ;;         ((ruby-mode ruby-ts-mode)                . (lambda () (require 'dap-ruby)))
;; ;;         ((go-mode go-ts-mode)                    . (lambda () (require 'dap-go)))
;; ;;         ((java-mode java-ts-mode jdee-mode)      . (lambda () (require 'dap-java)))
;; ;;         ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-lldb)))
;; ;;         ((objc-mode swift-mode)                  . (lambda () (require 'dap-lldb)))
;; ;;         (php-mode                                . (lambda () (require 'dap-php)))
;; ;;         (elixir-mode                             . (lambda () (require 'dap-elixir)))
;; ;;         ((js-mode js2-mode js-ts-mode)           . (lambda () (require 'dap-chrome)))
;;          (powershell-mode                         . (lambda () (require 'dap-pwsh))))
;; init (when (executable-find "python3")
;;        (setq dap-python-executable "python3")))


;; ;; C/C++/Objective-C
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
;;   :config
;;   (with-no-warnings
;;     ;; FIXME: fail to call ccls.xref
;;     ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
;;     (cl-defmethod my-lsp-execute-command
;;       ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
;;       (when-let ((xrefs (lsp--locations-to-xref-items
;;                          (lsp--send-execute-command (symbol-name command) arguments))))
;;         (xref--show-xrefs xrefs nil)))
;;     (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

;; ;; Swift
;; (use-package lsp-sourcekit)

;; Julia
;;(use-package lsp-julia
;;  :hook (julia-mode . (lambda () (require 'lsp-julia))))

;; ;; Java
;; (use-package lsp-java
;;   :hook ((java-mode java-ts-mode jdee-mode) . (lambda () (require 'lsp-java)))))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
