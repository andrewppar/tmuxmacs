;;; tmux-pane.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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
(require 'tmux-window)

(defun tmux-current-pane-id ()
  "Get the current tmux pane."
  (car (tmux-command-output-lines "display-message" "-p" "#{pane_id}")))

(defun tmux-pane/list
    ;; TODO: make WINDOW-NAME-OR-ID into WINDOW-OR-SESSION
    (&optional window-name-or-id with-window? with-session?)
  "List all tmux panes ids optionally for WINDOW-NAME-OR-ID.
Option: WITH-WINDOW? Provide window information for each pane.
Option: WITH-SESSION? Provide session information for each pane."
  (let ((window (tmux-window/find window-name-or-id))
	(format-args '())
	(args nil))
    (when with-session?
      (push "#{session_name}" format-args)
      (push "#{session_id}" format-args))
    (when with-window?
      (push "#{window_name}" format-args)
      (push "#{window_id}" format-args))
    (push "#{pane_id}" format-args)
    (setq format-args (string-join format-args " "))
    (setq args (list "-F" format-args))
    (if window
	(progn
	  (push window args)
	  (push "-t" args))
      (push "-a" args))
    (push "list-panes" args)
    (mapcar
     (lambda (line)
       (let ((clean-line (string-trim line)))
	 (cond ((and with-window? with-session?)
		(cl-destructuring-bind
		      (pane window-id window-name session-id session-name)
		    (split-string clean-line)
		  (list pane (list window-id window-name)
			(list session-id session-name))))
	       ((or with-window? with-session?)
		(cl-destructuring-bind (pane name id)
		    (split-string clean-line)
		  (list pane (list name id))))
	       (t clean-line))))
     (apply #'tmux-command-output-lines args))))

(defun tmux-pane/find (pane-id)
  "Get the pane corresponding to PANE-ID."
  (when (member pane-id (tmux-pane/list))
    pane-id))

(defun tmux-pane/context (pane-id)
  "Get any windows and sessions associated with PANE-ID."
  (let ((panes (tmux-command-output-lines
		"list-panes" "-a" "-F" "#{pane_id} #{window_id} #{session_id}"))
	(result nil))
    (dolist (line panes)
      (cl-destructuring-bind (pane window session)
	  (split-string line " ")
	(when (equal pane-id pane)
	  (push (cons window session) result))))
    result))

(defun tmux-pane/send-command (pane-id command &rest args)
  "Execute COMMAND with ARGS in PANE-ID."
  (let ((command-args (list
		       (string-join (cons command args) " ")
		       "ENTER")))
    (push pane-id command-args)
    (push "-t" command-args)
    (push "send-keys" command-args)
    (apply #'tmux-command-output command-args)))

(defun tmux-pane/split (&optional pane-id horizontal?)
  "Split PANE-ID vertically unless HORIZONTAL? is non-nil."
  (let* ((all-panes (tmux-list-panes))
	 (pane (or pane-id (tmux-current-pane-id)))
	 (args (list "-t" pane))
	 (new-pane nil))
    (when horizontal?
      (push "-h" args))
    (push "split-window" args)
    (apply #'tmux-command-output args)
    (dolist (pane (tmux-list-panes))
      (unless (member pane all-panes)
	(setq new-pane pane)))
    new-pane))

(defun tmux-pane/window-and-session (&optional pane-id)
  "Return a pair of the window and session for PANE-ID.

If PANE-ID is not supplied, the current pane is used."
  (let ((pane (or (tmux-pane/find pane-id) (tmux-current-pane-id)))
	(lines (tmux-command-output-lines
		"list-panes" "-a" "-F" "#{pane_id} #{window_id} #{session_id}"))
	(result nil))
    (dolist (line lines)
      (cl-destructuring-bind (maybe-pane window session)
	  (split-string line)
	(when (equal maybe-pane pane)
	  (setq result (list window session)))))
    result))

(provide 'tmux-pane)
;;; tmux-pane.el ends here
