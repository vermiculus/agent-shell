;;; agent-shell-list.el --- List interface for agent-shell buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; URL: https://github.com/xenodium/agent-shell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a `tabulated-list-mode' interface for managing multiple
;; agent-shell instances, similar to `list-packages' or `list-processes'.
;;
;; Use `agent-shell-list' to open the list buffer.
;;
;; Note: This package is in very early stages and likely has
;; rough edges.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'tabulated-list)
(require 'map)
(require 's)

(defvar agent-shell-prefer-viewport-interaction)

(declare-function magit-get-current-branch "magit-git")
(declare-function magit-get "magit-git")

(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell--project-name "agent-shell-project")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-open-transcript "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--get-model-name "agent-shell")
(declare-function agent-shell--get-mode-name "agent-shell")
(declare-function agent-shell-cycle-session-mode "agent-shell")
(declare-function agent-shell-set-session-mode "agent-shell")
(declare-function agent-shell-set-session-model "agent-shell")

(defvar agent-shell-list--refresh-timer nil
  "Timer for auto-refreshing the agent shell list.")

(defvar agent-shell-list--branch-cache (make-hash-table :test #'equal)
  "Cache for branch descriptions, keyed by directory path.
Each value is a cons cell (TIMESTAMP . DESCRIPTION).")

(defconst agent-shell-list--branch-cache-ttl 60
  "Time-to-live for branch cache entries in seconds.")

(defvar agent-shell-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'agent-shell-list-refresh)
    (define-key map (kbd "RET") #'agent-shell-list-goto-default)
    (define-key map (kbd "o") #'agent-shell-list-goto-shell)
    (define-key map (kbd "O") #'agent-shell-list-goto-viewport)
    (define-key map (kbd "t") #'agent-shell-list-open-transcript)
    (define-key map (kbd "N") #'agent-shell-list-new-shell)
    (define-key map (kbd "k") #'agent-shell-list-kill)
    ;; Session commands (consistent with agent-shell-help-menu)
    (define-key map (kbd "m") #'agent-shell-list-cycle-session-mode)
    (define-key map (kbd "M") #'agent-shell-list-set-session-mode)
    (define-key map (kbd "v") #'agent-shell-list-set-session-model)
    (define-key map (kbd "C") #'agent-shell-list-interrupt)
    map)
  "Keymap for `agent-shell-list-mode'.")

(define-derived-mode agent-shell-list-mode tabulated-list-mode "Agent Shells"
  "Major mode for listing agent-shell buffers.

\\{agent-shell-list-mode-map}"
  (setq tabulated-list-format
        [("Project" 20 t)
         ("Agent" 15 t)
         ("Model" 15 t)
         ("Mode" 12 t)
         ("Status" 6 t)
         ("Turns" 7 (lambda (a b)
                      (< (string-to-number (aref (cadr a) 5))
                         (string-to-number (aref (cadr b) 5)))))
         ("Last Active" 12 agent-shell-list--sort-by-activity)
         ("Branch" 25 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Last Active" . t))
  (add-hook 'tabulated-list-revert-hook #'agent-shell-list--refresh nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-list--stop-timer nil t)
  (tabulated-list-init-header))

(defun agent-shell-list--format-time-ago (time)
  "Format TIME as a human-readable relative time string.

Returns rounded approximations like \"just now\", \"2m ago\", \"1h ago\",
\"3d ago\".  We use our own implementation rather than `format-seconds'
because that function provides exact durations (e.g., \"1h 2m 5s\") rather
than the rounded single-unit approximations appropriate for a status display."
  (if (null time)
      "never"
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (minutes (/ seconds 60))
           (hours (/ minutes 60))
           (days (/ hours 24)))
      (cond
       ((< seconds 60) "just now")
       ((< minutes 60) (format "%dm ago" (truncate minutes)))
       ((< hours 24) (format "%dh ago" (truncate hours)))
       (t (format "%dd ago" (truncate days)))))))

(defun agent-shell-list--sort-by-activity (a b)
  "Sort entries A and B by last activity time (most recent first)."
  (let ((time-a (get-text-property 0 'agent-shell-list-time (aref (cadr a) 6)))
        (time-b (get-text-property 0 'agent-shell-list-time (aref (cadr b) 6))))
    (cond
     ((and (null time-a) (null time-b)) nil)
     ((null time-a) t)   ; nil sorts after real times
     ((null time-b) nil)
     (t (time-less-p time-a time-b)))))

(defun agent-shell-list--fetch-branch-description (directory)
  "Fetch the git branch description for DIRECTORY.
This performs the actual git calls.  Use
`agent-shell-list--get-branch-description' for cached access."
  (let ((default-directory directory))
    (if-let ((branch (magit-get-current-branch)))
        (let ((lines (s-lines (s-trim
                               (or (magit-get "branch" branch "description")
                                   (propertize branch 'font-lock-face 'magit-branch-local))))))
          (if (cdr lines)
              (concat (car lines) "...")
            (car lines)))
      "")))

(defun agent-shell-list--get-branch-description ()
  "Get the git branch description or branch name for the shell's directory.

Returns the value of git config branch.BRANCH.description if set,
otherwise returns the branch name, or an empty string if not in a git repo.
Multi-line descriptions are truncated to the first line with an ellipsis.

Results are cached per directory for `agent-shell-list--branch-cache-ttl'
seconds to avoid repeated git calls during auto-refresh."
  (let* ((directory (agent-shell-cwd))
         (cached (gethash directory agent-shell-list--branch-cache))
         (now (float-time)))
    (if (and cached
             (< (- now (car cached)) agent-shell-list--branch-cache-ttl))
        (cdr cached)
      (let ((description (agent-shell-list--fetch-branch-description directory)))
        (puthash directory (cons now description)
                 agent-shell-list--branch-cache)
        description))))

(defun agent-shell-list--has-pending-permission-p (state)
  "Return non-nil if STATE has any tool call awaiting permission."
  (when-let ((tool-calls (map-elt state :tool-calls)))
    (seq-some (lambda (tool-call-entry)
                (map-elt (cdr tool-call-entry) :permission-request-id))
              tool-calls)))

(defun agent-shell-list--get-status (state)
  "Derive the status string from STATE.

Returns a propertized string:
- \"ask\" if any tool calls are awaiting permission
- \"think\" if heartbeat status is started or busy
- \"idle\" otherwise"
  (cond
   ((agent-shell-list--has-pending-permission-p state)
    (propertize "ask" 'face 'warning))
   ((memq (map-nested-elt state '(:heartbeat :status)) '(started busy))
    (propertize "think" 'face 'font-lock-keyword-face))
   (t
    (propertize "idle" 'face 'success))))

(defun agent-shell-list--entries ()
  "Generate entries for the agent shell list."
  (mapcar
   (lambda (buffer)
     (with-current-buffer buffer
       (let* ((state (agent-shell--state))
              (config (map-elt state :agent-config))
              (agent-name (or (map-elt config :mode-line-name)
                              (map-elt config :buffer-name)
                              "Unknown"))
              (model-name (or (agent-shell--get-model-name state) ""))
              (mode-name (or (agent-shell--get-mode-name state) ""))
              (project (or (agent-shell--project-name) ""))
              (branch (agent-shell-list--get-branch-description))
              (status (agent-shell-list--get-status state))
              (request-count (or (map-elt state :request-count) 0))
              (last-activity (map-elt state :last-activity-time))
              (activity-str (propertize
                             (agent-shell-list--format-time-ago last-activity)
                             'agent-shell-list-time last-activity)))
         (list buffer
               (vector project
                       agent-name
                       model-name
                       mode-name
                       status
                       (number-to-string request-count)
                       activity-str
                       branch)))))
   (agent-shell-buffers)))

(defun agent-shell-list--clear-branch-cache ()
  "Clear the branch description cache.
Called on explicit refresh to ensure fresh data."
  (clrhash agent-shell-list--branch-cache))

(defun agent-shell-list--refresh ()
  "Refresh the agent shell list entries."
  (setq tabulated-list-entries (agent-shell-list--entries)))

(defun agent-shell-list-refresh ()
  "Refresh the agent shell list, clearing the branch cache."
  (interactive)
  (agent-shell-list--clear-branch-cache)
  (revert-buffer))

(defun agent-shell-list--start-timer ()
  "Start the auto-refresh timer."
  (unless agent-shell-list--refresh-timer
    (setq agent-shell-list--refresh-timer
          (run-with-timer 1 1 #'agent-shell-list--timer-refresh))))

(defun agent-shell-list--stop-timer ()
  "Stop the auto-refresh timer."
  (when agent-shell-list--refresh-timer
    (cancel-timer agent-shell-list--refresh-timer)
    (setq agent-shell-list--refresh-timer nil)))

(defun agent-shell-list--timer-refresh ()
  "Refresh the list if the buffer is visible.
Preserves scroll position unless the set of agent buffers has changed."
  (when-let ((buf (get-buffer "*Agent Shells*")))
    (when (get-buffer-window buf)
      (with-current-buffer buf
        (let* ((old-buffers (mapcar #'car tabulated-list-entries))
               (new-buffers (agent-shell-buffers))
               (buffers-changed (not (equal (sort old-buffers)
                                            (sort new-buffers)))))
          (if buffers-changed
              (revert-buffer)
            ;; Preserve scroll position when only updating existing entries
            (let ((window (get-buffer-window buf))
                  (point-pos (point))
                  (window-start-pos (window-start (get-buffer-window buf))))
              (revert-buffer)
              (set-window-start window window-start-pos)
              (goto-char point-pos))))))))

(defun agent-shell-list--on-shell-created ()
  "Refresh the agent shell list when a new shell is created.
Added to `agent-shell-mode-hook'."
  (agent-shell-list--timer-refresh))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'agent-shell-list--on-shell-created))

(defun agent-shell-list--get-buffer-at-point ()
  "Get the agent-shell buffer at point."
  (tabulated-list-get-id))

;;;###autoload
(defun agent-shell-list ()
  "Display a list of all agent-shell buffers."
  (interactive)
  (let ((buf (get-buffer-create "*Agent Shells*")))
    (with-current-buffer buf
      (agent-shell-list-mode)
      (agent-shell-list--refresh)
      (tabulated-list-print)
      (agent-shell-list--start-timer))
    (pop-to-buffer buf)))

(defun agent-shell-list-goto-default ()
  "Switch to the agent buffer at point, respecting viewport preference."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (if agent-shell-prefer-viewport-interaction
        (agent-shell-list-goto-viewport)
      (agent-shell-list-goto-shell))))

(defun agent-shell-list-goto-shell ()
  "Switch to the shell buffer at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (pop-to-buffer buffer)))

(defun agent-shell-list-goto-viewport ()
  "Switch to the viewport buffer for the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (if-let ((viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer buffer)))
        (pop-to-buffer viewport-buffer)
      (user-error "No viewport buffer available"))))

(defun agent-shell-list-open-transcript ()
  "Open the transcript for the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (with-current-buffer buffer
      (agent-shell-open-transcript))))

(defun agent-shell-list-interrupt ()
  "Interrupt the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (with-current-buffer buffer
      (agent-shell-interrupt))
    (revert-buffer)))

(defun agent-shell-list-cycle-session-mode ()
  "Cycle through session modes for the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (with-current-buffer buffer
      (agent-shell-cycle-session-mode))
    (revert-buffer)))

(defun agent-shell-list-set-session-mode ()
  "Set session mode for the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (with-current-buffer buffer
      (agent-shell-set-session-mode))
    (revert-buffer)))

(defun agent-shell-list-set-session-model ()
  "Set session model for the agent at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (with-current-buffer buffer
      (agent-shell-set-session-model))
    (revert-buffer)))

(defun agent-shell-list--shells-in-directory (directory)
  "Return list of agent shell buffers with cwd matching DIRECTORY."
  (let ((dir (expand-file-name directory)))
    (seq-filter (lambda (buffer)
                  (string= dir (expand-file-name
                                (with-current-buffer buffer
                                  (agent-shell-cwd)))))
                (agent-shell-buffers))))

(defun agent-shell-list-new-shell ()
  "Start a new agent shell, prompting for project and agent type.
Warns if there are existing shells in the selected project."
  (interactive)
  (let* ((default-directory (project-prompt-project-dir))
         (existing (agent-shell-list--shells-in-directory default-directory)))
    (when existing
      (unless (yes-or-no-p
               (format "%d shell(s) already exist in %s. Create another? "
                       (length existing)
                       (abbreviate-file-name default-directory)))
        (user-error "Aborted")))
    (let ((config (agent-shell-select-config :prompt "New agent: ")))
      (agent-shell--dwim :config config :new-shell t))))

(defun agent-shell-list-kill ()
  "Kill the agent buffer at point."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (when (yes-or-no-p (format "Kill agent shell %s? " (buffer-name buffer)))
      (kill-buffer buffer)
      (revert-buffer))))

(provide 'agent-shell-list)
;;; agent-shell-list.el ends here
