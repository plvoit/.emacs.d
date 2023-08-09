(define-minor-mode double-commander-mode
  "Toggles global dotcrafter-mode."
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

(defun double-commander ()
 "Opens two dired windows. Like Double Commander."
 (interactive)
 (double-commander-mode 1)
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
  (double-commander-mode 1)
  (dired "/home/voit")
  (dired-other-window "/ssh:voit@login1.hpc.uni-potsdam.de:/work/voit"))


(defun double-commander-kill-all-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (kill-buffer buffer)))))

(global-set-key (kbd "<f8>") 'double-commander)
(global-set-key (kbd "<f9>") 'double-commander-remote)

(provide 'init-double-commander)
