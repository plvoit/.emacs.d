;;; agent-shell-diff-edit.el --- Editable proposed-version buffer for agent-shell diffs -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds an `e' keybinding to agent-shell-diff-mode that opens the proposed
;; content for editing.  C-c C-c writes the edited version to disk and rejects
;; the diff (so Claude won't overwrite it).  Use `agent-shell-diff-edit-reread'
;; afterwards to tell Claude to re-read the file.

;;; Code:

(require 'agent-shell-diff)

;; ---------------------------------------------------------------------------
;; Buffer-local vars injected into the diff buffer via advice

(defvar-local agent-shell-diff-edit--old-content nil
  "Original file content captured from the :old arg of `agent-shell-diff'.")

(defvar-local agent-shell-diff-edit--new-content nil
  "Proposed file content captured from the :new arg of `agent-shell-diff'.")

(defvar-local agent-shell-diff-edit--shell-buffer nil
  "The agent-shell buffer that spawned this diff buffer.")

;; ---------------------------------------------------------------------------
;; Buffer-local vars in the edit buffer

(defvar-local agent-shell-diff-edit--diff-buffer nil
  "The diff buffer that spawned this edit buffer.")

(defvar-local agent-shell-diff-edit--saved-wconf nil
  "Window configuration saved just before the edit buffer was displayed.")

;; ---------------------------------------------------------------------------
;; Last file written (used by reread command)

(defvar agent-shell-diff-edit--last-file nil
  "Path of the last file written by `agent-shell-diff-edit-confirm'.")

;; ---------------------------------------------------------------------------
;; Advice: capture :old/:new content and shell buffer in every diff buffer

(defun agent-shell-diff-edit--around-diff (orig &rest args)
  "Capture :old/:new args and calling buffer in the new diff buffer."
  (let ((calling-buffer (current-buffer))
        (buf (apply orig args)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq agent-shell-diff-edit--old-content (plist-get args :old)
              agent-shell-diff-edit--new-content (plist-get args :new)
              agent-shell-diff-edit--shell-buffer calling-buffer)
        (setq header-line-format
              (concat (or header-line-format "") "  e edit"))))
    buf))

(advice-add 'agent-shell-diff :around #'agent-shell-diff-edit--around-diff)

;; ---------------------------------------------------------------------------
;; e keybinding

(define-key agent-shell-diff-mode-map (kbd "e") #'agent-shell-diff-edit)

;; ---------------------------------------------------------------------------
;; Commands

(defun agent-shell-diff-edit ()
  "Open the proposed file version in an editable buffer.

Press \\[agent-shell-diff-edit-confirm] to save the edited version or
\\[agent-shell-diff-edit-cancel] to cancel."
  (interactive)
  (unless agent-shell-diff-edit--new-content
    (user-error "No proposed content captured — was the diff created before agent-shell-diff-edit loaded?"))
  (let* ((diff-buffer (current-buffer))
         (file agent-shell-diff--file)
         (new-content agent-shell-diff-edit--new-content)
         (edit-buf-name (format "*agent-shell-edit: %s*"
                                (file-name-nondirectory file)))
         (is-new (null (get-buffer edit-buf-name)))
         (edit-buf (get-buffer-create edit-buf-name))
         (saved-wconf (current-window-configuration)))
    (with-current-buffer edit-buf
      (when is-new
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert new-content))
        ;; Derive major mode from file extension without triggering mode hooks
        ;; (delay-mode-hooks prevents auto-preview hooks like markdown rendering)
        (let ((buffer-file-name file))
          (delay-mode-hooks (set-auto-mode)))
        (setq buffer-read-only nil)
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "C-c C-c") #'agent-shell-diff-edit-confirm)
        (local-set-key (kbd "C-c C-k") #'agent-shell-diff-edit-cancel)
        (setq header-line-format "  C-c C-c save+reject  C-c C-k cancel"))
      (setq agent-shell-diff-edit--diff-buffer diff-buffer
            agent-shell-diff-edit--saved-wconf saved-wconf))
    (pop-to-buffer edit-buf)))

(defun agent-shell-diff-edit-cancel ()
  "Cancel editing and restore the window configuration."
  (interactive)
  (let ((wconf agent-shell-diff-edit--saved-wconf))
    (kill-current-buffer)
    (when wconf
      (set-window-configuration wconf))))

(defun agent-shell-diff-edit-confirm ()
  "Write the edited content to disk and reject the diff.

Rejects the diff so Claude won't overwrite the file, then notifies
Claude to re-read the saved file."
  (interactive)
  (let* ((edited-content (buffer-substring-no-properties (point-min) (point-max)))
         (diff-buffer agent-shell-diff-edit--diff-buffer)
         (wconf agent-shell-diff-edit--saved-wconf))
    (unless (buffer-live-p diff-buffer)
      (user-error "Associated diff buffer is no longer live"))
    ;; Read everything from diff-buffer before any destructive operations
    (let* ((file (with-current-buffer diff-buffer agent-shell-diff--file))
           (original-new (with-current-buffer diff-buffer
                           agent-shell-diff-edit--new-content))
           (shell-buffer (with-current-buffer diff-buffer
                           agent-shell-diff-edit--shell-buffer)))
      (kill-current-buffer)
      (when wconf
        (set-window-configuration wconf))
      (if (string= edited-content original-new)
          ;; No changes — accept as normal
          (with-current-buffer diff-buffer
            (agent-shell-diff-accept-all))
        ;; Write to the exact path the agent would have used, no prompts
        (write-region edited-content nil file nil 'silent)
        (when-let (buf (find-buffer-visiting file))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert edited-content)
              (set-buffer-modified-p nil))))
        (setq agent-shell-diff-edit--last-file file)
        ;; Reject the diff so Claude won't overwrite our file
        (with-current-buffer diff-buffer
          (agent-shell-diff-reject-all))
        ;; Notify Claude to re-read the file (deferred so reject completes first)
        (when (buffer-live-p shell-buffer)
          (run-with-timer
           0 nil
           (lambda ()
             (when (buffer-live-p shell-buffer)
               (with-current-buffer shell-buffer
                 (agent-shell--send-command
                  :prompt (format "I applied a modified version of your proposed edit to `%s`. Please re-read the file and continue."
                                  file)
                  :shell-buffer shell-buffer))))))))))

(defun agent-shell-diff-edit-reread ()
  "Tell the agent to re-read the last file saved via `agent-shell-diff-edit-confirm'."
  (interactive)
  (unless agent-shell-diff-edit--last-file
    (user-error "No file to re-read (use `e' in a diff buffer first)"))
  (agent-shell--send-command
   :prompt (format "I applied a modified version of your proposed edit to `%s`. Please re-read the file and continue."
                   agent-shell-diff-edit--last-file)
   :shell-buffer (current-buffer)))

(provide 'agent-shell-diff-edit)

;;; agent-shell-diff-edit.el ends here
