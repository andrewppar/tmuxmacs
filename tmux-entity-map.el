;;; tmux-entity-map.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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

(defun tmux--make-entity-map ()
  "Make an empty entity map."
  '())

(defun tmux--entity-map-key (session-id window-id)
  "Make an entity-map-key for SESSION-ID and WINDOW-ID."
  (cons session-id window-id))

(defun tmux--entity-map-make-entry (session-id window-id pane-ids)
  "Make a tmux entity map entry for SESSION-ID WINDOW-ID and PANE-IDS."
  (let ((key (tmux--entity-map-key session-id window-id)))
    (cons key pane-ids)))

(defun tmux--entity-map-add (entity-map session-id window-id pane-id)
  "Add an entry for PANE-ID at SESSION-ID and WINDOW-ID in ENTITY-MAP."
  (let ((result (tmux--make-entity-map))
	(found? nil))
    (dolist (binding entity-map)
      (cl-destructuring-bind
	    ((binding-session-id . binding-window-id) . panes)
	  binding
	(if (and (equal binding-session-id session-id)
		 (equal binding-window-id window-id))
	    (progn
	      (push (tmux--entity-map-make-entry
		     session-id window-id (cons pane-id panes))
		    result)
	      (setq found? t))
	  (push binding result))))
    (unless found?
      (push (tmux--entity-map-make-entry
	     session-id window-id (list pane-id))
	    result))
    result))

(defun tmux--entity-map-get (entity-map session-id &optional window-id)
  "Get the entry for SESSION-ID in ENTITY-MAP.

If WINDOW-ID is provided only the panes for WINDOW-ID are returned."
  (let ((result '()))
    (dolist (binding entity-map)
      (cl-destructuring-bind
	    ((binding-session-id . binding-window-id) . panes)
	  binding
	(when (equal session-id binding-session-id)
	  (if window-id
	      (when (equal window-id binding-window-id)
		(push (cons binding-window-id panes) result))
	    (push (cons binding-window-id panes) result)))))
    result))

(defun tmux-current-entity-map ()
  "Get a representation of the current tmux server."
  (let ((result (tmux--make-entity-map)))
    (dolist (line (tmux-command-output-lines
		   "list-panes" "-a" "-F" "#{session_id} #{window_id} #{pane_id}"))
      (cl-destructuring-bind (session-id window-id pane-id)
	  (split-string line)
	(setq result
	      (tmux--entity-map-add result session-id window-id pane-id))))
    result))

(provide 'tmux-entity-map)
;;; tmux-entity-map.el ends here
