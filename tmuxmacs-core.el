;;; tmuxmacs-core.el -- make calls to tmux -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: gtd
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 1.0.0

;;; Commentary:

;; execute tmux commands from within Emacs

;;; Code:

(defun tmuxmacs-core--output (fields)
  (let ((json-body '()))
    (dolist (field fields)
      (let ((field-name (substring (format "%s" field) 1)))
	(push (format "\\\"%s\\\": \\\"#{%s}\\\"" field-name field-name) json-body)))
    (format "{%s}" (string-join json-body ","))))

(defun tmuxmacs-core/execute (command &rest fields)
  (let* ((command-base (format "tmux %s" command))
	 (command (if fields
		      (format "%s -F \"%s\"" command-base (tmuxmacs-core--output fields))
		    command-base))
	 (response (shell-command-to-string command)))
    (json-parse-string
     (format "[%s]" (string-join (string-split response "\n" t " ") ","))
   :object-type 'plist
   :array-type 'list
   :false-object nil)))

(defun tmuxmacs-core/entity-map ()
  (tmuxmacs-core/execute
   "list-panes -a"
   :pane_id :pane_current_command :window_id :window_name :session_id :session_name))

(defun tmuxmacs-core/lookup (key value)
  (seq-some
   (lambda (pane-data) (when (equal (plist-get pane-data key) value) pane-data))
   (tmuxmacs-core/entity-map)))

(defun tmuxmacs-core/id-type (identifier)
  (cond ((string-prefix-p "$" identifier) :session)
	((string-prefix-p "@" identifier) :window)
	((string-prefix-p "%" identifier) :pane)))

(provide 'tmuxmacs-core)
;;; tmuxmacs-core.el ends here
