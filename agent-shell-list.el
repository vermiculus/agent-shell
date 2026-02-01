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

(defvar agent-shell-prefer-viewport-interaction)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell--project-name "agent-shell-project")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-open-transcript "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell--dwim "agent-shell")

(defvar agent-shell-list--refresh-timer nil
  "Timer for auto-refreshing the agent shell list.")

(defvar agent-shell-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-list-goto-default)
    (define-key map (kbd "o") #'agent-shell-list-goto-shell)
    (define-key map (kbd "v") #'agent-shell-list-goto-viewport)
    (define-key map (kbd "t") #'agent-shell-list-open-transcript)
    (define-key map (kbd "i") #'agent-shell-list-interrupt)
    (define-key map (kbd "N") #'agent-shell-list-new-shell)
    (define-key map (kbd "k") #'agent-shell-list-kill)
    map)
  "Keymap for `agent-shell-list-mode'.")

(define-derived-mode agent-shell-list-mode tabulated-list-mode "Agent Shells"
  "Major mode for listing agent-shell buffers.

\\{agent-shell-list-mode-map}"
  (setq tabulated-list-format
        [
         ;; Claude Code, Codex, etc
         ("Agent" 15 t)
         ;; Project name
         ("Project" 20 t)
         ;; Current working directory
         ("Directory" 30 t)
         ;; Idle, Thinking, Needs Permission
         ("Status" 18 t)
         ;; How many prompts have been sent -- proxy for context rot
         ("Prompts" 7 (lambda (a b)
                        (< (string-to-number (aref (cadr a) 4))
                           (string-to-number (aref (cadr b) 4)))))
         ;; Recent activity (buffer modification tick)
         ("Activity" 10 (lambda (a b)
                          (< (string-to-number (aref (cadr a) 5))
                             (string-to-number (aref (cadr b) 5)))))])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Activity" . t))  ; descending by activity
  (add-hook 'tabulated-list-revert-hook #'agent-shell-list--refresh nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-list--stop-timer nil t)
  (tabulated-list-init-header))

(defun agent-shell-list--has-pending-permission-p (state)
  "Return non-nil if STATE has any tool calls awaiting permission."
  (when-let ((tool-calls (map-elt state :tool-calls)))
    (seq-some (lambda (tool-call-entry)
                (map-elt (cdr tool-call-entry) :permission-request-id))
              tool-calls)))

(defun agent-shell-list--get-status (state)
  "Derive the status string from STATE.

Returns a propertized string:
- \"Needs Permission\" if any tool calls are awaiting permission
- \"Thinking\" if heartbeat status is busy
- \"Idle\" otherwise"
  (cond
   ((agent-shell-list--has-pending-permission-p state)
    (propertize "Needs Permission" 'face 'warning))
   ((eq (map-nested-elt state '(:heartbeat :status)) 'busy)
    (propertize "Thinking" 'face 'font-lock-keyword-face))
   (t
    (propertize "Idle" 'face 'success))))

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
              (project (or (agent-shell--project-name) ""))
              (directory (or (agent-shell-cwd) ""))
              (status (agent-shell-list--get-status state))
              (request-count (or (map-elt state :request-count) 0))
              (activity (buffer-modified-tick)))
         (list buffer
               (vector agent-name
                       project
                       directory
                       status
                       (number-to-string request-count)
                       (number-to-string activity))))))
   (agent-shell-buffers)))

(defun agent-shell-list--refresh ()
  "Refresh the agent shell list entries."
  (setq tabulated-list-entries (agent-shell-list--entries)))

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
  "Refresh the list if the buffer is visible."
  (when-let ((buf (get-buffer "*Agent Shells*")))
    (if (get-buffer-window buf)
        (with-current-buffer buf
          (let ((pos (point)))
            (revert-buffer)
            (goto-char pos)))
      ;; Buffer not visible, stop timer
      (agent-shell-list--stop-timer))))

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
      (message "No viewport buffer available"))))

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
      (agent-shell-interrupt))))

(defun agent-shell-list-new-shell ()
  "Start a new agent shell in the same project as the agent at point.

Uses the same agent type as the selected shell."
  (interactive)
  (when-let ((buffer (agent-shell-list--get-buffer-at-point)))
    (let* ((state (with-current-buffer buffer (agent-shell--state)))
           (config (map-elt state :agent-config))
           (default-directory (with-current-buffer buffer
                                (agent-shell-cwd))))
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
