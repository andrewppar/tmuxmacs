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

(defun tmux-list-panes (&optional window-name-or-id)
  "List all tmux panes ids optionally for WINDOW-NAME-OR-ID."
  (let ((window-id (tmux-window-get window-name-or-id))
	(args (list "-F" "#{pane_id}")))
    (if window-id
	(progn
	  (push window-id args)
	  (push "-t" args))
      (push "-a" args))
    (push "list-panes" args)
    (mapcar
     #'string-trim
     (apply #'tmux-command-output-lines args))))

(defun tmux-pane-get (pane-id)
  "Get the pane corresponding to PANE-ID."
  (when (member pane-id (tmux-list-panes))
    pane-id))

(defun tmux-current-pane-id ()
  "Get the current tmux pane."
  (car (tmux-command-output-lines "display-message" "-p" "#{pane_id}")))

(defun tmux-split-pane (&optional pane-name-or-id horizontal?)
  "Split PANE-NAME-OR-ID vertically unless HORIZONTAL? is non-nil."
  (let* ((all-panes (tmux-list-panes))
	 (pane-id (or (tmux-pane-get pane-name-or-id) (tmux-current-pane)))
	 (args (list "-t" pane-id))
	 (new-pane nil))
    (when horizontal?
      (push "-h" args))
    (push "split-window" args)
    (apply #'tmux-command-output args)
    (dolist (pane (tmux-list-panes))
      (unless (member pane all-panes)
	(setq new-pane pane)))
    new-pane))

(defun tmux-send-command (pane-id command &rest args)
  "Execute COMMAND with ARGS in PANE-ID."
  (let ((command-args (list
		       (string-join (cons command args) " ")
		       "ENTER")))
    (push pane-id command-args)
    (push "-t" command-args)
    (push "send-keys" command-args)
    (apply #'tmux-command-output command-args)))

(provide 'tmux-pane)
;;; tmux-pane.el ends here
