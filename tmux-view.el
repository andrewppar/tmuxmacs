;;; tmux-view.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: tmux
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:

;; Emacs views over tmux information.
;;

;; Does this mean that I think Emacs is the best OS? Yes.

;;; Code:
(require 'cl)
(require 'tmux-pane)

(defun tmux-buffer--make-line-string (line max-column)
  "Create a string for LINE with padding for each column to match MAX-COLUMN."
  (let ((result ""))
    (dolist (column line)
      (let ((padding (make-string (+ (- max-column (length column)) 1) ?\ )))
	(setq result (concat result column padding))))
    (concat result "\n")))

(defun tmux-buffer--insert-lines (buffer columns lines limit offset)
  "Insert COLUMNS and LIMIT LINES at OFFSET into BUFFER."
  (switch-to-buffer buffer)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  (let ((max-column (apply
		     #'max
		     (mapcar
		      #'length
		      (apply #'append (cons columns lines)))))
	(column-count (length columns)))

    (insert (tmux-buffer--make-line-string columns max-column))
    (insert (concat (make-string (* max-column column-count) ?\=) "\n"))
    (let ((idx 0)
	  (todo (mapcar #'identity lines)))
      (while (and (< idx (length lines)) todo)
	(when (<= offset idx limit)
	  (insert (tmux-buffer--make-line-string (car todo) max-column)))
	(setq todo (cdr todo)
	      idx  (+ 1 idx))))
    (read-only-mode 1)))

(defun tmux-buffer--get-user-input (buffer columns lines limit offset)
  "Update BUFFER with COLUMNS and LINES with LIMIT as a limit at OFFSET."
  (let ((input-char (read-char
		     "Use j and k to scroll, and any other key to quit."))
	(max-offset (- (length lines) 1)))
    (cond ((equal input-char ?\j)
	   (let ((new-offset (if (= offset max-offset) max-offset (+ offset 1))))
	     (tmux-buffer--insert-lines buffer columns lines limit new-offset)
	     (tmux-buffer--get-user-input buffer columns lines limit new-offset)))
	  ((equal input-char ?\k)
	   (let ((new-offset (if (= offset 0) offset (- offset 1))))
	     (tmux-buffer--insert-lines buffer columns lines limit new-offset)
	     (tmux-buffer--get-user-input buffer columns lines limit new-offset)))
	  (t
	   (kill-buffer buffer)))))

(defmacro tmux-buffer-view (columns lines)
  "Given LINES with COLUMNS display a small buffer with that information."
  (let* ((buffer (gensym))
	 (root   (gensym))
	 (root-height (gensym))
	 (window (gensym))
	 (line-count (gensym)))
    `(save-window-excursion
       (let* ((,root (frame-root-window))
	      (,root-height (window-height ,root))
	      (,window (split-window ,root
				     (- ,root-height
					(/ ,root-height 3)))))
	 (select-window ,window)
	 (let ((,buffer (switch-to-buffer "*tmux-view*" t t))
	       (,line-count (window-screen-lines)))
	   (tmux-buffer--insert-lines ,buffer ,columns ,lines ,line-count 0)
	   (read-only-mode 1)
	   (tmux-buffer--get-user-input ,buffer ,columns ,lines ,line-count 0))))))

(defun tmux-view-panes (&optional window-id-or-name)
  ;; TODO: make WINDOW-ID-OR-NAME into WINDOW-OR-SESSION
  "View all tmux panes.  Optionally limit to WINDOW-ID-OR-NAME."
  (interactive)
  (let ((pane-data (tmux-pane/list window-id-or-name t t))
	(columns '("pane id" "window" "window id" "session" "session id"))
	(panes '()))
    (dolist (pane-datum pane-data)
      (cl-destructuring-bind
	    (pane (window-id window) (session-id session))
	  pane-datum
	(push (list pane window window-id session session-id) panes)))
    (tmux-buffer-view columns panes)))

(defun tmux-view-windows (&optional session-id-or-name)
  "View all tmux windows.  Optionally limit to SESSION-ID-OR-NAME."
  (interactive)
  (let ((window-data (tmux-window/list (tmux-session/find session-id-or-name) t))
	(columns (list "window name" "window id" "session id")))
    (tmux-buffer-view columns window-data)))

(defun tmux-view-sessions ()
  "View tmux sessions."
  (interactive)
  (tmux-buffer-view
   '("session name" "session id")
   (mapcar (lambda (session-datum)
	     (list (car session-datum) (cdr session-datum)))
	   (tmux-session/list))))

(provide 'tmux-view)
;;; tmux-view.el ends here
