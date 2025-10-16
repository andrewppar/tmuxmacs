;;; tmuxmacs-session.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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
(require 'tmuxmacs-core)

(defun tmuxmacs-session--id (session-name)
  (plist-get
   (tmuxmacs-core/lookup :session_name session-name)
   :session_id))

(defun tmuxmacs-session--name (session-name)
  (plist-get
   (tmuxmacs-core/lookup :session_id session-name)
   :session_id))

(defun tmuxmacs-session/focus (session-id)
  (tmuxmacs-core/execute (format "switch -t '%s'" session-id)))

(defun tmuxmacs-session/focused ()
  (let ((key :session_id)
	(command "display-message -p"))
    (plist-get (car (tmuxmacs-core/execute command key)) key)))

(defun tmuxmacs-session/new (&optional session-name)
  (let ((command "new-session -d -P"))
    (when (and session-name (not (equal (string-trim session-name) "")))
      (setq command (format "%s -s %s" command session-name)))
    (tmuxmacs-core/execute command :session_id)))

(defun tmuxmacs-session/rename (session-id new-name)
  (tmuxmacs-core/execute
   (format "rename-session -t '%s' '%s'" session-id new-name)))

(defun tmuxmacs-session/find (session-name-or-id)
  (when session-name-or-id
    (or (tmuxmacs-session--id session-name-or-id)
	(tmuxmacs-session--name session-name-or-id))))

(defun tmuxmacs-session/kill (session-id)
  (tmuxmacs-core/execute (format "kill-session -t '%s'" session-id)))

(provide 'tmuxmacs-session)
;;; tmuxmacs-session.el ends here
