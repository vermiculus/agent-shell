;;; my-agent-shell-worktree.el --- Worktree-based parallel agent shells  -*- lexical-binding: t; -*-

;; Experimental function for test-driving worktree-based parallel agent processing.

;;; Code:

(require 'magit)
(require 'project)

(defun my/agent-shell-new-worktree (project description)
  "Create a new git worktree with branch and start an agent shell there.

Prompts for PROJECT and DESCRIPTION.  The branch name is derived from
DESCRIPTION.  The worktree is created as a sibling directory to the
project's git repository.

This enables parallel agent processing by having each agent work
in its own isolated worktree."
  (interactive
   (list (project-prompt-project-dir)
         (read-string "Branch description (short, used for branch name): ")))
  (let* ((default-directory project)
         (repo-root (or (magit-toplevel)
                        (user-error "Cannot create worktree; project is not a git repository: %s" project)))
         (repo-name (file-name-nondirectory (directory-file-name repo-root)))
         (sanitized-desc (replace-regexp-in-string
                          "[^a-zA-Z0-9]+" "-"
                          (downcase (string-trim description))))
         (branch-name (concat "agent/" sanitized-desc))
         (worktree-parent (file-name-directory (directory-file-name repo-root)))
         (worktree-path (let (path)
                          ;; find an unused worktree path
                          (while (file-exists-p
                                  (setq path (expand-file-name
                                              (concat repo-name "--agent-"
                                                      (format "%08x" (random (expt 16 8))))
                                              worktree-parent))))
                          path))
         (config (or agent-shell-preferred-agent-config
                     (agent-shell-select-config :prompt "Select agent: "))))
    ;; Create the worktree with new branch
    (magit-worktree-branch worktree-path branch-name (magit-get-current-branch))
    ;; Start agent shell in the new worktree
    (let ((default-directory worktree-path))
      (agent-shell-start :config config))
    (message "Created worktree %s with branch %s" worktree-path branch-name)))

(provide 'my-agent-shell-worktree)
;;; my-agent-shell-worktree.el ends here
