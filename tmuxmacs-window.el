;;; tmuxmacs-window.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: tmux
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Manage tmux window from within Emacs

;;; Code:
(require 'tmuxmacs-core)
(require 'tmuxmacs-session)
(require 'cl-lib)

(defun tmw--quote (item)
  (format "'%s'" item))

(cl-defun tmuxmacs-window/new (&key session name directory command)
  ;; TODO: support command
  (let* ((session-id (tmw--quote
		      (or (tmuxmacs-session/find session)
			  (tmuxmacs-session/focused))))
	 (args (reverse
		(seq-reduce
		 (lambda (result optional-arg)
		   (let ((flag (car optional-arg))
			 (value (cdr optional-arg)))
		     (cond ((equal flag "-c")
			    (cons (or value "$HOME") (cons flag result)))
			   ((equal flag "-n")
			    (if value (cons (tmw--quote value) (cons flag result)) result))
			   (t result))))
		 (list (cons "-n" name) (cons "-c" directory))
		 (reverse (list "new-window" "-t" session-id))))))
    (tmuxmacs-core/execute (string-join args " "))))

(defun tmuxmacs-window/session (window-id)
  (plist-get (tmuxmacs-core/lookup :window_id window-id) :session_id))

(defun tmuxmacs-window/focus (window-id)
  (let ((session (tmuxmacs-window/session window-id)))
    (tmuxmacs-session/focus session)
    (tmuxmacs-core/execute (format "select-window -t '%s'" window-id))))

(defun tmuxmacs-window/rename (window-id new-name)
  (tmuxmacs-core/execute
   (format "rename-window -t '%s' '%s'" window-id new-name)))

(defun tmuxmacs-window/kill (window-id)
  (tmuxmacs-core/execute (format "kill-window -t '%s'" window-id)))

(provide 'tmuxmacs-window)
;;; tmuxmacs-window.el ends here
