(define-minor-mode double-commander-mode
  "Toggles global double commander mode."
  nil   ; Initial value, nil for disabled
  :global t
  ;; :group 'dotfiles
  ;; :lighter " dotcrafter"
  ;; :keymap
  ;; (list (cons (kbd "C-c C-. t") (lambda ()
  ;;                             (interactive)
  ;;                             (message "dotcrafter key binding used!"))))

  (if double-commander-mode
      (progn
        ;; Code to run when the mode is enabled
        (setq dired-kill-when-opening-new-dired-buffer nil))
    ;; Code to run when the mode is disabled
    (setq  dired-kill-when-opening-new-dired-buffer 1)))

(add-hook 'double-commander-mode-hook (lambda () (message "Hook was executed!")))
(add-hook 'double-commander-mode-on-hook (lambda () (message "double-commander turned on!")))
(add-hook 'double-commander-mode-off-hook (lambda () (message "double-commander turned off!")))

;;this would be nice to implement at some point. Stops dired from creating a new buffer when moving back in the filetree
;; from https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;;prevent updir to create a new buffer
;; (eval-after-load "dired"
;;  ;; don't remove `other-window', the caller expects it to be there
;;   '(defun dired-up-directory (&optional other-window)
;;     "Run Dired on parent directory of current directory."
;;        (interactive "P")
;;        (let* ((dir (dired-current-directory))
;;        (orig (current-buffer))
;;        (up (file-name-directory (directory-file-name dir))))
;;        (or (dired-goto-file (directory-file-name dir))
;;        ;; Only try dired-goto-subdir if buffer has more than one dir.
;;        (and (cdr dired-subdir-alist)
;;        (dired-goto-subdir up))
;;        (progn
;;      	  (kill-buffer orig)
;;      	  (dired up)
;;      	  (dired-goto-file dir))))))



(defun double-commander ()
  "Opens two dired windows. Like Double Commander."
  (interactive)
  (if double-commander-mode
      (progn
        (double-commander-mode -1)
        (message "DC off")
	(kill-all-dired-buffers)
	(delete-other-windows))
    (progn
      (double-commander-mode 1)
      (message "DC on")
      (dired "/home/voit")
      (dired-other-window "/home/voit"))
      ))


(defun kill-all-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (kill-buffer buffer)))))

(defun double-commander-remote ()
  "Opens two dired windows. Like Double Commander."
  (interactive)
  (if double-commander-mode
      (progn
        (double-commander-mode -1)
        (message "DC off")
	(kill-all-dired-buffers)
	(delete-other-windows))
    (progn
      (double-commander-mode 1)
      (message "DC on")
      (dired "/home/voit")
      (dired-other-window "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))
      ))


(global-set-key (kbd "<f8>") 'double-commander)
(global-set-key (kbd "<f9>") 'double-commander-remote)


(provide 'init-double-commander)
