# Setup for Emacs on HPC

Emacs does not run on GUI on HPC, just in Terminal. Also, the connection to the MELPA-package server does not work. Setting up a python IDE with Elpy therefore has to be done manually:

1. Go to the Elpy homepage and follow the instructions for manual install:7
   https://elpy.readthedocs.io/en/latest/introduction.html#installation

   To install the dependencies, go to the Melpa homepage https://melpa.org/#/?q=better and search for the packages. Each of the packages has a link for Download via github.
   Clone the git-repositories.

2. To read all the dependencies and to enable elpy follow the installation manual but also include the load paths for the dependencies in the init.el. An example is following in this file.
3. The pyvenv needs to be installed via pip: pip install virtualenv
   Once Elpy starts it will create its virtual environment.
4. Running just one line has  a really complicated shortcut and the Ctrl-Enter combination does not work for some reason. Therefore, I added a line in the init.el and put the key binding to Ctrl-r

## Example init.el

(add-to-list 'load-path "/home/voit/.emacs.d/elpy")
(add-to-list 'load-path "/home/voit/.emacs.d/company-mode")
(add-to-list 'load-path "/home/voit/.emacs.d/Highlight-Indentation-for-Emacs")
(add-to-list 'load-path "/home/voit/.emacs.d/pyvenv")
(add-to-list 'load-path "/home/voit/.emacs.d/s.el")
(add-to-list 'load-path "/home/voit/.emacs.d/yasnippet")
(add-to-list 'load-path "/home/voit/.emacs.d/better-defaults")

;; workon home
(setenv "WORKON_HOME" "/home/voit/.conda/envs/")

(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally

;;==========================
;; Color themes
;; Make Elisp files in that directory available to the user.
;;(add-to-list 'load-path "~/.emacs.d/ef-themes-1.1.1")

;; Load the theme of choice:
(load-theme 'ef-dark :no-confirm)
(load "better-defaults")
(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")

;; Enable elpy
(elpy-enable)

(define-key elpy-mode-map (kbd "C-r") 'elpy-shell-send-statement-and-step)
