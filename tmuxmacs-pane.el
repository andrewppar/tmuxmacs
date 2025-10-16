;;; tmuxmacs-pane.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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
(require 'tmuxmacs-window)
(require 'cl-lib)

(defun tmuxmacs-pane/list ()
  (tmuxmacs-core/entity-map))

(defun tmuxmacs-pane/focused ()
  (car (tmuxmacs-core/execute "display-message -p" :pane_id)))


(defun tmuxmacs-pane/find (pane-id)
  "Get the pane corresponding to PANE-ID."
  (when (member pane-id (tmuxmacs-pane/list))
    pane-id))

(defun tmuxmacs-pane/window (pane-id)
  (seq-some
   (lambda (pane)
     (cl-destructuring-bind (&key pane_id window_id &allow-other-keys)
	 pane
       (when (equal pane-id pane_id)
	 window_id)))
   (tmuxmacs-pane/list)))

(defun tmuxmacs-pane/focus (pane-id)
  (tmuxmacs-window/focus (tmuxmacs-pane/window pane-id))
  (tmuxmacs-core/execute (format "select-pane -t %s" pane-id)))

(defun tmuxmacs-pane/send-command (pane-id command)
  (let* ((escaped-command (string-replace "'" "\\'" command))
	 (to-send (format "send-keys -t '%s' '%s' ENTER" pane-id escaped-command)))
    (tmuxmacs-core/execute to-send)))

(defun tmuxmacs-pane/session (pane-id)
  (plist-get (tmuxmacs-core/lookup :pane_id pane-id) :session_id))

(defun tmuxmacs-pane/kill (pane-id)
  (tmuxmacs-core/execute (format "kill-pane -t '%s'" pane-id)))

(provide 'tmuxmacs-pane)
;;; tmuxmacs-pane.el ends here
