;;; tmuxmacs.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: tmux
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:

;; Control a tmux session (even the one Emacs is in)
;; from within Emacs.

;; Does this mean that I think Emacs is the best OS? Yes.

;;; Code:
(require 'tmux-session)
(require 'tmux-window)
(require 'tmux-pane)
(require 'tmux-view)

(defun tmux--get-session ()
  "Get session id from a user interactively."
  (let ((session->id (tmux-session/list)))
    (alist-get
     (completing-read "Select a session " session->id nil t)
     session->id
     nil nil #'equal)))

(defun tmux--get-window ()
  "Prompt user to select a window."
  (let ((window->id (tmux-window/list)))
    (car
     (alist-get
      (completing-read "Select a window " window->id)
      window->id
      nil nil #'equal))))

(defun tmux--get-pane ()
  "Prompt user to select a pane."
  (let* ((panes (mapcar
		 (lambda (pane-info)
		   (cl-destructuring-bind (pane (_ window) (_ session))
		       pane-info
		     (format "%s in %s:%s" pane window session)))
		 (tmux-pane/list nil t t)))
	 (pane (completing-read "Select a pane " panes nil t)))
    (car (split-string pane))))

(defun tmuxmacs--get-arg-value (arg-prefix args)
  "Get the arg with ARG-PREFIX from ARGS."
  (let ((result nil))
    (dolist (arg args)
      (when (string-prefix-p arg-prefix arg)
	(setq result (cadr (split-string arg "=")) )))
    result))

;;; Pane
;;;###autoload
(defun tmuxmacs/send-pane-command ()
  "Send a command to a tmux pane."
  (interactive)
  (let ((pane (tmux--get-pane))
	(command-string (read-string "Command: ")))
    (cl-destructuring-bind (command &rest args)
	(split-string command-string)
      (apply #'tmux-pane/send-command pane command args))))

(defun tmuxmacs/kill-pane ()
  "Kill a pane."
  (interactive)
  (let ((pane (tmux--get-pane)))
    (tmux-pane/kill pane)))

;;;###autoload
(defun tmuxmacs/send-pane (&optional args)
  "Move a pane to another window with optional ARGS."
  (interactive (list (transient-args 'tmux-pane-send-transient)))
  (let* ((horizontal? (some (lambda (arg) (equal arg "--horizontal")) args))
	 (pane (tmux--get-pane))
	 (window (tmux--get-window)))
    (tmux-pane/to-window pane window horizontal?)))

;;; Window

;;;###autoload
(defun tmuxmacs/create-window (&optional args)
  "Create a new tmux window with optional ARGS.

Only intended to be called from a transient menu."
  (interactive (list (transient-args 'tmux-window-create-transient)))
  (let ((session (tmuxmacs--get-arg-value "--session" args))
	(name (tmuxmacs--get-arg-value "--name" args))
	(command (tmuxmacs--get-arg-value "--eval" args)))
    (if command
	(save-tmux-excursion
	  (let* ((window (tmux-window/make name session))
		 (pane   (car (tmux-pane/list window))))
	    (apply #'tmux-pane/send-command pane (split-string command))))
      (tmux-window/make name session))))

(defun tmuxmacs/focus-window ()
  "Switch focus to a window interactively."
  (interactive)
  (tmux-window/focus (tmux--get-window)))

(defun tmuxmacs/kill-window ()
  "Kill tmux window interactively."
  (interactive)
  (tmux-window/kill (tmux--get-window)))

(defun tmuxmacs/rename-window ()
  "Rename tmux window interactively."
  (interactive)
  (let ((window (tmux--get-window))
	(name (read-string "Window name: ")))
    (tmux-window/rename window name)))

(defun tmuxmacs/send-window ()
  "Send window to session interactively."
  (interactive)
  (let ((window (tmux--get-window))
	(session (tmux--get-session)))
    (tmux-window/to-session window session)))

;;; session

;;;###autoload
(defun tmuxmacs/create-session (&optional args)
  "Create a new tmux session with optional ARGS.

Only intended to be called from a transient menu."
  (interactive (list  (transient-args 'tmux-window-create-transient)))
  (let ((name (tmuxmacs--get-arg-value "--name" args)))
    (tmux-session/make name)))

(defun tmuxmacs/focus-session ()
  "Set focus to tmux session interactively."
  (interactive)
  (tmux-session/focus (tmux--get-session)))

(defun tmuxmacs/kill-session ()
  "Kill a tmux session interactively."
  (interactive)
  (tmux-session/kill (tmux--get-session)))

(defun tmuxmacs/rename-session ()
  "Rename a tmux session interactively."
  (interactive)
  (let ((session (tmux--get-session))
	(name (read-string "New session name: ")))
    (tmux-session/rename session name)))

;;; transients

(transient-define-prefix tmux-pane-send-transient ()
  "Send a pane."
  ["Arguments"
   ("h" "horizontal split?" "--horizontal")]
  ["send pane"
   ("s" "send to window" tmuxmacs/send-pane)])

(transient-define-prefix tmux-pane-transient ()
  "Tmuxmacs panes."
  ["panes"
   ("c" "command" tmuxmacs/send-pane-command)
   ("k" "kill" tmuxmacs/kill-pane)
   ("s" "send to window" tmux-pane-send-transient)
   ("v" "view pane information" tmux-view-panes)])

(transient-define-infix name-option ()
  :description "Add a name to an entity"
  :class 'transient-option
  :shortarg "n"
  :argument "--name=")

(transient-define-infix session-option ()
  :description "Select a session"
  :class 'transient-option
  :shortarg "s"
  :argument "--session="
  :choices (when (tmux-active?) (tmux-session/list)))

(transient-define-infix command-option ()
  :description "Send a command"
  :class 'transient-option
  :shortarg "e"
  :argument "--eval=")

(transient-define-prefix tmux-window-create-transient ()
  "Create a window."
  ["Arguments"
   (name-option)
   (session-option)
   (command-option)]
  ["window create"
   ("c" "create" tmuxmacs/create-window)])

(transient-define-prefix tmux-session-create-transient ()
  ["Arguments"
   (name-option)]
  ["session create"
   ("c" "create" tmuxmacs/create-session)])

(transient-define-prefix tmux-window-transient ()
  "Tmuxmacs windows."
  ["windows"
   ("c" "create" tmux-window-create-transient)
   ("f" "focus" tmuxmacs/focus-window)
   ("k" "kill" tmuxmacs/kill-window)
   ("r" "rename" tmuxmacs/rename-window)
   ("s" "send" tmuxmacs/send-window)
   ("v" "view window information" tmux-view-windows)])

(transient-define-prefix tmux-session-transient ()
  "Tmuxmacs sessions."
  ["sessions"
   ("c" "create" tmux-session-create-transient)
   ("f" "focus" tmuxmacs/focus-session)
   ("k" "kill" tmuxmacs/kill-session)
   ("r" "rename" tmuxmacs/rename-session)
   ("v" "view session information" tmux-view-sessions)])

(transient-define-prefix tmuxmacs-transient ()
  "Control tmux from Emacs."
  ["tmux"
   ("p" "pane" tmux-pane-transient)
   ("s" "session" tmux-session-transient)
   ("w" "window" tmux-window-transient)])

;;;###autoload
(defun tmuxmacs ()
  (interactive)
  (tmuxmacs-transient))

(provide 'tmuxmacs)
;;; tmuxmacs.el ends here
