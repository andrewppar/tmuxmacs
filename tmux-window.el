;;; tmux-window.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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
(require 'tmux-session)

(defun tmux-list-windows (&optional session)
  "List all tmux windows as (name . id).

If SESSION is given restrict to SESSION otherwise all windows in all
sessions are listed."
  (let ((args (list "-F" "#{window_name} #{window_id}")))
    (if session
	(setq args (cons "-t" (cons session args)))
      (setq args (cons "-a" args)))
    (setq args (cons "list-windows" args))
    (apply #'tmux-command->alist args)))

(defun tmux-window-id-by-name (name)
  "Get the id for the tmux window with NAME."
  (alist-get name (tmux-list-windows) nil nil #'equal))

(defun tmux-switch-window-by-id (window-id)
  "Switch tmux focus to window with WINDOW-ID."
  (tmux-command-output "select-window" "-t" window-id))

(defun tmux-switch-window-by-name (window-name)
  "Swith to the tmux window with WINDOW-NAME."
  (when-let ((id (tmux-window-id-by-name window-name)))
    (tmux-switch-window-by-id id)))

(defun tmux-switch-window (window-name-or-id)
  "Switch to the tmux window with WINDOW-NAME-OR-ID."
  (or (tmux-switch-window-by-name window-name-or-id)
      (tmux-switch-window-by-id window-name-or-id)))

(defmacro save-tmux-excursion (&rest body)
  "Execute BODY returning to the current tmux window."
  (declare (indent 0))
  (let ((current-session (gensym))
	(current-window  (gensym)))
    `(let ((,current-session (tmux-current-session-id))
	   (,current-window  (tmux-current-window-id)))
       ,@body
       (tmux-switch-session ,current-session)
       (tmux-switch-window ,current-window))))

(defun tmux-current-window-id ()
  "Return the window-id for the current tmux window."
  (car
   (tmux-command-output-lines "display-message" "-p" "#{window_id}")))

(defun tmux-new-window (&optional name session-id)
  "Create a new tmux window with NAME for SESSION-ID or the current session."
  (let ((session (or session-id (tmux-current-session-id)))
	(current-windows (tmux-list-windows session-id)))
    (save-tmux-excursion
      (tmux-switch-session session)
      (if name
	  (tmux-command-output "new-window" "-n" name)
	(tmux-command-output "new-window")))
    ;; This is definitely not thread safe...
    (let ((new-windows (tmux-list-windows session-id))
	  (new-window-id   nil))
      (if name
	  (setq new-window-id (alist-get name new-windows nil nil #'equal))
	(cl-do ((todo new-windows (cdr new-windows))
		(key (caar new-windows) (caar todo))
		(value (cdar new-windows) (cdar todo)))
	       ((or new-window-id (not key)) new-window-id)
	  (when (not (alist-get key current-windows nil nil #'equal))
	    (setq new-window-id value))))
      new-window-id)))

(defun tmux-window-get (window-name-or-id)
  "Get the window id for WINDOW-NAME-OR-ID."
  (let ((window-list (tmux-list-windows)))
    (or (alist-get window-name-or-id window-list nil nil #'equal)
	(when (member window-name-or-id (mapcar #'cdr window-list))
	  window-name-or-id))))

(defun tmux-window-session (window-id)
  "Get the session associated with WINDOW-ID."
  (let ((window->session (tmux-command->alist
			  "list-windows" "-a" "-F" "#{window_id} #{session_id}")))
    (alist-get window-id window->session nil nil #'equal)))

(defun tmux-kill-window (window-name-or-id)
  "Kill tmux window with WINDOW-NAME-OR-ID."
  (when-let ((window-id (tmux-window-get window-name-or-id)))
    (tmux-command-output "kill-window" "-t" window-id)))

(provide 'tmux-window)
;;; tmux-window.el ends here
