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

;;;;;;;;;;;;;
;;; Utilities

(defun tmux-window-id-by-name (name)
  "Get the id for the tmux window with NAME."
  (car (alist-get name (tmux-window/list) nil nil #'equal)))

(defun tmux-focused-window-id ()
  "Return the window-id for the current tmux window."
  (car
   (tmux-command-output-lines "display-message" "-p" "#{window_id}")))

;;;;;;;;
;; Focus


(defun tmux-focus-window-by-id (window-id)
  "Switch tmux focus to window with WINDOW-ID."
  (let ((session (tmux-window/session window-id)))
    (tmux-session/focus session)
    (tmux-command-output "select-window" "-t" window-id)))

(defun tmux-focus-window-by-name (window-name)
  "Swith to the tmux window with WINDOW-NAME."
  (when-let ((id (tmux-window-id-by-name window-name)))
    (tmux-focus-window-by-id id)))

      ;;
;;;;;;;;

(defmacro save-tmux-excursion (&rest body)
  "Execute BODY returning to the current tmux window."
  (declare (indent 0))
  (let ((current-session (gensym))
	(current-window  (gensym)))
    `(let ((,current-session (tmux-current-session-id))
	   (,current-window  (tmux-focused-window-id)))
       ,@body
       (tmux-session/focus ,current-session)
       (tmux-window/focus ,current-window))))

          ;;;
;;;;;;;;;;;;;

(defun tmux-window/list (&optional session with-session-info?)
  "List all tmux windows as (name . id).

If SESSION is given restrict to SESSION otherwise all windows in all
sessions are listed.

If WITH-SESSION-INFO? is non-nil then include session_id in the output."
  (let ((format-string "#{window_name} #{window_id}"))
    (when with-session-info?
      (setq format-string (concat format-string " #{session_id}")))
    (let ((args (list "-F" format-string)))
      (if session
	  (setq args (cons "-t" (cons session args)))
	(setq args (cons "-a" args)))
      (setq args (cons "list-windows" args))
      (mapcar #'split-string (apply #'tmux-command-output-lines args)))))

(defun tmux-window/make (&optional name session-id)
  "Create a new tmux window with NAME for SESSION-ID or the current session."
  (let ((session (or (tmux-session/find session-id) (tmux-current-session-id)))
	(current-windows (tmux-window/list session-id))
	(args '("-P" "-F" "#{window_id}")))
    (when name
      (push name args)
      (push "-n" args))
    (when session
      (push session args)
      (push "-t" args))
    (push "new-window" args)
    (string-trim (apply #'tmux-command-output args))))

 (defun tmux-window/find (window-name-or-id)
  "Get the window id for WINDOW-NAME-OR-ID."
  (let ((window-list (tmux-window/list)))
    (or (car (alist-get window-name-or-id window-list nil nil #'equal))
	(when (member window-name-or-id (mapcar #'cadr window-list))
	  window-name-or-id))))

(defun tmux-window/find-or-make (name)
  "If window with NAME exists return it otherwise create it."
  (if-let ((window (tmux-window/find name)))
      window
    (tmux-window/make name)))

(defun tmux-window/session (window-id)
  "Get the session associated with WINDOW-ID."
  (let ((window->session (tmux-command->alist
			  "list-windows" "-a" "-F" "#{window_id} #{session_id}")))
    (alist-get window-id window->session nil nil #'equal)))

(defun tmux-window/rename (window-name-or-id new-name)
  "Rename WINDOW-NAME-OR-ID to NEW-NAME."
  (when-let ((window (tmux-window/find window-name-or-id)))
    (tmux-command-output "rename-window" "-t" window new-name)
    (tmux-window/find new-name)))

(defun tmux-window/focus (window-name-or-id)
  "Switch to the tmux window with WINDOW-NAME-OR-ID."
  (or (tmux-focus-window-by-name window-name-or-id)
      (tmux-focus-window-by-id window-name-or-id)))

(defun tmux-window/to-session (window-name-or-id session-name-or-id)
  "Move WINDOW-NAME-OR-ID to SESSION-NAME-OR-ID as a new window.

If SESSION-NAME-OR-ID does no exist it is created.

The session and window that the WINDOW-NAME-OR-ID has been moved to are
returned as `cl-values`."
  (let ((session (tmux-session/find-or-make session-name-or-id))
	(window  (tmux-window/find window-name-or-id)))
    (tmux-command-output "move-window" "-s" window "-t" session)
    (cl-values session window)))

(defun tmux-window/kill (window-name-or-id)
  "Kill tmux window with WINDOW-NAME-OR-ID."
  (when-let ((window-id (tmux-window/find window-name-or-id)))
    (tmux-command-output "kill-window" "-t" window-id)))

(provide 'tmux-window)
;;; tmux-window.el ends here
