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

;;;;;;;;;;;;;
;;; Utilities

(defun tmux-current-session-id ()
  "Get current session name."
  (car (tmux-command-output-lines "display-message" "-p" "#{session_id}")))

(defun tmux-session-id-by-name (name)
  "Get the id for the tmux session with NAME."
  (alist-get name (tmux-session/list) nil nil #'equal))

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

;;;;;;;;;
;; Focus

(defun tmux-focus-session-by-id (session-id)
  "Switch to tmux session with SESSION-ID."
  (tmux-command-output "switch" "-t" session-id))

(defun tmux-focus-session-by-name (session-name)
  "Switch to tmux session with SESSION-NAME."
  (when-let ((session-id (tmux-session/find session-name)))
    (tmux-focus-session-by-id session-id)))

       ;;
;;;;;;;;;

(defun tmux-session/list ()
  "List the currently running tmux sessions."
  (tmux-command->alist "ls" "-F" "#{session_name} #{session_id}"))

(defun tmux-session/focused ()
  "Return the id of the currently active tmux session."
  (tmux-current-session-id))

(defun tmux-session/make (&optional name)
  "Create a new tmux session optionally giving it a NAME."
  (let* ((old-sessions (mapcar #'cdr (tmux-session/list)))
	 (args '( "#{session_id}" "-F" "-P" "-d" "new-session")))
    (when name
      (setq args (cons name (cons "-s" args))))
    (setq args (reverse args))
    (string-trim (apply #'tmux-command-output args))))

(defun tmux-session/find (session-name-or-id)
  "Get the session id for SESSION-NAME-OR-ID."
  (or (tmux-session-id-by-name session-name-or-id)
      (when (member session-name-or-id (mapcar #'cdr (tmux-session/list)))
	session-name-or-id)))

(defun tmux-session/find-or-make (name)
  "If session with NAME exists return it otherwise create it."
  (if-let ((session (tmux-session/find name)))
      session
    (tmux-session/make name)))

(defun tmux-session/rename (session-name-or-id new-name)
  "Rename session with SESSION-NAME-OR-ID to NEW-NAME."
  (when-let ((session (tmux-session/find session-name-or-id)))
    (tmux-command-output "rename-session" "-t" session new-name)
    (tmux-session/find new-name)))

(defun tmux-session/focus (session-name-or-id)
  "Switch to the tmux session with SESSION-NAME-OR-ID."
  (or (tmux-focus-session-by-name session-name-or-id)
      (tmux-focus-session-by-id session-name-or-id)))

(defun tmux-session/kill (session-id)
  "Kill the tmux session with SESSION-ID."
  (tmux-command-output "kill-session" "-t" session-id))

(provide 'tmux-session)
;;; tmux-session.el ends here
