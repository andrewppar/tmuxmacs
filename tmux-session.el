;;; tmux-session.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: tmux
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Manage tmux sessions from within Emacs

;;; Code:
(require 'tmux-interface)

(defun tmux-kill-session (session-id)
  "Kill the tmux session with SESSION-ID."
  (tmux-command-output "kill-session" "-t" session-id))

(defun tmux-current-session-id ()
  "Get current session name."
  (car (tmux-command-output-lines "display-message" "-p" "#{session_id}")))

(defun tmux-list-sessions ()
  "List the currently running tmux sessions."
  (tmux-command->alist "ls" "-F" "#{session_name} #{session_id}"))

(defun tmux-new-session (&optional name)
  "Create a new tmux session optionally giving it a NAME."
  (let* ((old-sessions (mapcar #'cdr (tmux-list-sessions)))
	 (args '("-d" "new-session"))
	 (new-session-id nil))
    (when name
      (setq args (cons name (cons "-s" args))))
    (setq args (reverse args))
    (apply #'tmux-command-output args)
    (dolist (session-id (mapcar #'cdr (tmux-list-sessions)))
      (unless (member session-id old-sessions)
	(setq new-session-id session-id)))
    new-session-id))

(defun tmux-session-id-by-name (name)
  "Get the id for the tmux session with NAME."
  (alist-get name (tmux-list-sessions) nil nil #'equal))

(defun tmux-session-windows (&optional session-id)
  "List all windows for a tmux session.
SESSION-ID is optional.  If it is not passed the current session id is used."
  (let ((session (or session-id (tmux-current-session-id))))
  (mapcar
   (lambda (window-line)
     (cl-destructuring-bind (window-name window-id)
	 (split-string window-line)
       (cons window-name window-id)))
   (tmux-command-output-lines
    "list-windows" "-t" session "-F" "#{window_name} #{window_id}"))))

(defun tmux-session-get (session-name-or-id)
  "Get the session id for SESSION-NAME-OR-ID."
  (or (tmux-session-id-by-name session-name-or-id)
      (when (member session-name-or-id (mapcar #'cdr (tmux-list-sessions)))
	session-name-or-id)))

(defun tmux-switch-session-by-id (session-id)
  "Switch to tmux session with SESSION-ID."
  (tmux-command-output "switch" "-t" session-id))

(defun tmux-switch-session-by-name (session-name)
  "Switch to tmux session with SESSION-NAME."
  (when-let ((session-id (tmux-session-get session-name)))
    (tmux-switch-session-by-id session-id)))

(defun tmux-switch-session (session-name-or-id)
  "Switch to the tmux session with SESSION-NAME-OR-ID."
  (or (tmux-switch-session-by-name session-name-or-id)
      (tmux-switch-session-by-id session-name-or-id)))


(provide 'tmux-session)
;;; tmux-session.el ends here
