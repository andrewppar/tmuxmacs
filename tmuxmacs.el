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
(require 'tmuxmacs-view)
(require 'tmuxmacs-session)
(require 'tmuxmacs-window)
(require 'tmuxmacs-pane)

(defmacro tmuxmacs--with-buffer-refresh (&rest body)
  `(progn
     (progn ,@body)
     (tmuxmacs-view/sessions)))

(defmacro tmuxmacs/save-excursion (&rest body)
  (let ((pane (gensym))
	(result (gensym)))
    `(let ((,pane (plist-get (tmuxmacs-pane/focused) :pane_id))
	   (,result (progn ,@body)))
       (tmuxmacs-pane/focus ,pane)
       ,result)))

;;;###autoload
(defun tmuxmacs ()
  (interactive)
  (tmuxmacs-view/sessions))

(defun tmuxmacs/focus ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (tmuxmacs-core/execute
    (format "switch -t '%s'" (tmuxmacs-view/id-at-point)))))

(defun tmuxmacs/rename ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (let* ((old-name (tmuxmacs-view/name-at-point))
	  (id (tmuxmacs-view/id-at-point))
	  (item-type (tmuxmacs-view/type-at-point))
	  (item-type-name (substring (format "%s" item-type) 1))
	  (new-name (read-string
		     (format "new name for %s: " item-type-name)
		     old-name)))
     (pcase item-type
       (:session (tmuxmacs-session/rename id new-name))
       (:window (tmuxmacs-window/rename id new-name))))))

(defun tmuxmacs/new-session ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (let ((new-name (read-string "new session name: ")))
     (tmuxmacs-session/new new-name))))

(defun tmuxmacs--prompt (prompt)
  (let ((result (read-string prompt)))
    (unless (equal (string-trim result) "")
      result)))

(defun tmuxmacs/new-window ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (let ((id (tmuxmacs-view/id-at-point))
	 (name (tmuxmacs--prompt "window name: ")))
     (when-let* ((session (pcase (tmuxmacs-core/id-type id)
			   (:session id)
			   (:window (tmuxmacs-window/session id))
			   (:pane (tmuxmacs-pane/session id)))))
       (tmuxmacs-window/new :session session :name name)))))

(defun tmuxmacs/kill ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (let* ((id (tmuxmacs-view/id-at-point))
	  (id-type (tmuxmacs-core/id-type id))
	  (id-name (tmuxmacs-view/name-at-point))
	  (id-type-name (substring (format "%s" id-type) 1))
	  (prompt (format "really kill %s %s with id %s?" id-type-name id-name id)))
     (when (y-or-n-p prompt)
       (pcase id-type
	 (:session (tmuxmacs-session/kill id))
	 (:window (tmuxmacs-window/kill id))
	 (:pane (tmuxmacs-pane/kill id)))))))

(defun tmuxmacs/send-command ()
  (interactive)
  (let ((pane-id (tmuxmacs-view/id-at-point))
	(tail-showing? (tmuxmacs-view/pane-tail-showing?)))
    (tmuxmacs--with-buffer-refresh
     (let ()
       (if (equal (tmuxmacs-core/id-type pane-id) :pane)
	   (let ((command (read-string "command: ")))
	     (tmuxmacs-pane/send-command pane-id command))
	 (message "Point is not at a pane."))))
    (tmuxmacs-view/goto-id pane-id)
    (when tail-showing?
      (tmuxmacs-view/toggle-pane-tail))
    (tmuxmacs-view/goto-id pane-id)))

(defun tmuxmacs/move-window ()
  (interactive)
  (tmuxmacs--with-buffer-refresh
   (let ((window-id (tmuxmacs-view/id-at-point)))
     (if (equal (tmuxmacs-core/id-type window-id) :window)
	 (let ((session-id (cdr (tmuxmacs-view/session-selection))))
	   (tmuxmacs-window/move window-id session-id))
       (warn "Point is not at a window")))))

(defmacro tmuxmacs/save-excursion (&rest body)
  (let ((window-id (gensym)))
    `(let ((,window-id (tmuxmacs-window/focused)))
       (progn ,@body)
       (tmuxmacs-window/focus ,window-id))))

(defun tmuxmacs/move-pane ()
  (interactive)
  (tmuxmacs/save-excursion
   (tmuxmacs--with-buffer-refresh
    (let ((pane-id (tmuxmacs-view/id-at-point)))
      (if (equal (tmuxmacs-core/id-type pane-id) :pane)
	  (let ((window-id (cdr (tmuxmacs-view/window-selection))))
	    (tmuxmacs-pane/move pane-id window-id :horizontal? t))
	(warn "point is not at a pane"))))))


(defun tmuxmacs/pane-tail ()
  (interactive)
  (tmuxmacs-view/toggle-pane-tail))

(defun tmuxmacs/pane-tail-refresh ()
  (interactive)
  (tmuxmacs-view/pane-tail-refresh))

(defun tmuxmacs--formatted-panes ()
  (mapcar
   (lambda (pane)
     (cl-destructuring-bind
	   (&key pane_id window_id window_name session_name session_id &allow-other-keys)
	 pane
       (format "%s [%s] < %s"
	       pane_id (or window_name window_id) (or session_name session_id))))
   (tmuxmacs-pane/list)))

(defun tmuxmacs/pane-send-command ()
  (interactive)
  (let* ((panes (tmuxmacs--formatted-panes))
	 (selected-pane (car (split-string (completing-read "select a pane: " panes nil t))))
	 (command (read-string "command: ")))
    (tmuxmacs-pane/send-command selected-pane command)))

(provide 'tmuxmacs)
;;; tmuxmacs.el ends here
