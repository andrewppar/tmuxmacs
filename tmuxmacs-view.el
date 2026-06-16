;;; tmuxmacs-view.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'tmuxmacs-pane)
(require 'subr-x)
(require 'tmuxmacs-face)

(define-derived-mode tmuxmacs-mode fundamental-mode
  "tmuxmacs"
  "Major mode for viewing tmux."
  (define-key tmuxmacs-mode-map
      (kbd "C-c q") (lambda () (interactive) (kill-buffer (current-buffer)))))

(defun tmv--plist-get-in (plist keys)
  "Retrieve a value nested deep inside a plist using a sequence of KEYS.
PLIST is the property list to search.  KEYS is a list of keys specifying
the path to the desired value.  Each key is used to traverse one level
of the property list.  If a key is not found, the function returns nil."
  (let ((result plist))
    (dolist (key keys)
      (setq result (plist-get result key #'equal)))
    result))

(defun tmv--plist-put-in (plist keys value)
  "Recursively set a VALUE in PLIST for nested KEYS.
PLIST is the property list to modify.
KEYS is a list of keys representing the path in the PLIST where the
VALUE should be set.  If KEYS is empty, the original PLIST is
returned.  When KEYS contains only one element, the function modifies
PLIST directly to assign VALUE to the corresponding key.
Returns the modified property list."
  (cond
    ((not keys)
     plist)
    ((= (length keys) 1)
     (plist-put plist (car keys) value #'equal))
    (t
     (let ((rest-keys (cdr keys))
	   (inner-list (plist-get plist (car keys) #'equal)))
       (plist-put
	plist (car keys) (tmv--plist-put-in inner-list rest-keys value) #'equal)))))

(defun tmv--plist-update-in (plist keys function &rest args)
  "Update the value in PLIST associated with the sequence of KEYS.
FUNCTION is applied to the current value, with ARGS passed as additional
arguments.  Uses `tmv--plist-get-in' to retrieve the current value and
`tmv--plist-put-in' to store the updated value."
  (tmv--plist-put-in
   plist keys (apply function (tmv--plist-get-in plist keys) args)))

(defun tmv--add-item (lista item)
  "Add ITEM to LISTA if it is not already a member of LISTA.
Iterates through the combined list of ITEM and LISTA, ensuring
no duplicates are added.  Returns the updated list."
  (seq-reduce
   (lambda (acc element) (if (member element acc) acc (cons element acc)))
   (cons item lista)
   '()))

(defun tmv--pane-data->session-data (pane-data)
  (let ((result '()))
    (dolist (pane pane-data)
      (cl-destructuring-bind (&key session_name session_id
				   window_name window_id
				   pane_current_command pane_id
				   &allow-other-keys)
	  pane
	(setq result
	      (thread-first
		result
		(tmv--plist-update-in (list session_id window_id :panes) #'tmv--add-item pane_id)
		(tmv--plist-put-in (list :names window_id) window_name)
		(tmv--plist-put-in (list :names session_id) session_name)
		(tmv--plist-put-in (list :names pane_id) pane_current_command)))))
    result))

(defun tmv--plist-keys (plist)
  "Extract the keys from PLIST (property list) and return them as a list.
The keys are extracted by selecting every second element in the plist,
starting from the first element (index 0)."
  (mapcar
   (lambda (idx) (nth idx plist))
   (number-sequence 0 (- (length plist) 1) 2)))


(defvar tmuxmacs-view--tmux-buffer-data nil)

(defmacro tmv--with-tmux-session-buffer (data &rest body)
  "Temporarily switch to the \"*tmux*\" buffer and execute BODY.
Within this context, set the variable `tmuxmacs-view--tmux-buffer-data` to
DATA.  Make the buffer writable during execution of BODY.  When done,
the buffer is set back to read-only as part of the `unwind-protect`
cleanup mechanism.

DATA is used to set the buffer-local variable `tmuxmacs-view--tmux-buffer-data`.
BODY is the sequence of forms that will be executed in the modified context."

  (declare (indent 1))
  `(unwind-protect
	(progn
          (switch-to-buffer "*tmux*")
	  (save-excursion
            (setq tmuxmacs-view--tmux-buffer-data ,data)
            (let ((inhibit-read-only t))
	      (erase-buffer)
	      (tmuxmacs-mode)
              (progn ,@body))))
     (read-only-mode 1)))

(defun tmv--format-line (id name face)
  "Format ID with NAME using FACE."
  (format "%s: %s"
	  (propertize id 'face face)
	  (propertize name 'face tmuxmacs-face/text)))

(defun tmv--format-pane (pane-id session-data last? last-window?)
  (let* ((pane-name (or (tmv--plist-get-in session-data (list :names pane-id))
			"unnamed pane"))
	 (pane-line (tmv--format-line pane-id pane-name tmuxmacs-face/pane)))
    (format "%s%s─ %s"
	    (if last-window? " " "│")
	    (if last? "╰" "├") pane-line)))

(defun tmv--format-window (session-id window-id session-data last?)
  (let* ((window-name (tmv--plist-get-in session-data (list :names window-id)))
	 (window-line (tmv--format-line window-id window-name tmuxmacs-face/window))
	 (panes (tmv--plist-get-in session-data (list session-id window-id :panes)))
	 (result-lines '()))
    (push (tmv--format-pane (car panes) session-data t last?) result-lines)
    (dolist (pane (cdr panes))
      (push (tmv--format-pane pane session-data nil last?) result-lines))

    (push (format "%s┬ %s" (if last? "╰" "├") window-line) result-lines)
    (string-join result-lines "\n")))

(defun tmv--format-session (session-id session-data)
  "Format a representation of a session.

SESSION-ID is the identifier for the session.
SESSION-DATA is a property list containing session details, including names
and window information.
The function returns a formatted string where the session ID and its name
appear first, followed by window IDs and their corresponding names."
  (let* ((names (plist-get session-data :names))
	 (windows (tmv--plist-keys (plist-get session-data session-id)))
	 (session-name (plist-get names session-id))
	 (session-line (format "%s: %s"
			       (propertize session-id 'face tmuxmacs-face/session)
			       (propertize session-name 'face tmuxmacs-face/text)))

	 (result '()))
    (push (tmv--format-window session-id (car windows) session-data t) result)
    (dolist (window-id (cdr windows))
      (push (tmv--format-window session-id window-id session-data nil) result))
    (push session-line result)
    (string-join result "\n")))

(defun tmuxmacs-view/sessions ()
  "Display a buffer listing tmux sessions.

This function retrieves the list of tmux panes and derives the corresponding
session data.  It then presents this information in a temporary buffer with
sessions formatted in a readable manner.  The buffer includes a header and
lists the sessions retrieved.

Each session is collected and displayed, skipping the :names key in the
session data."
  (let* ((sessions (tmv--pane-data->session-data (tmuxmacs-pane/list))))
    (tmv--with-tmux-session-buffer (list :origin sessions :current sessions)
      (insert
       (string-join
	(reverse
	 (seq-reduce
	  (lambda (acc session)
	    (if (equal session :names)
		acc
	      (cons (format "%s\n" (tmv--format-session session sessions)) acc)))
	  (tmv--plist-keys sessions)
	  (reverse
	   (list
	    (propertize "tmux sessions" 'face tmuxmacs-face/title)
	    (propertize "-------------\n" 'face tmuxmacs-face/divider)))))
	"\n")))))

(defun tmv--line ()
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun tmuxmacs-view/id-at-point ()
  (when-let ((prefix (car (split-string (tmv--line) ":" t " "))))
    (car (last (split-string  prefix " ")))))

(defun tmuxmacs-view/type-at-point ()
  (tmuxmacs-core/id-type (tmuxmacs-view/id-at-point)))

(defun tmuxmacs-view/name-at-point ()
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position) (line-end-position))))
    (string-trim (string-join (cdr (split-string line ":")) ":"))))

(defun tmuxmacs-view--selection (prompt name-key id-key)
  (let* ((name->id (mapcar
		    (lambda (session)
		      (cons (plist-get session name-key) (plist-get session id-key)))
		    (tmuxmacs-core/entity-map)))
	 (selection (completing-read prompt (mapcar #'car name->id) nil t)))
    (assoc selection name->id #'equal)))


(defun tmuxmacs-view/session-selection ()
  (tmuxmacs-view--selection "select session: " :session_name :session_id))

(defun tmuxmacs-view/window-selection ()
  (tmuxmacs-view--selection "select window: " :window_name :window_id))

(defun tmv--trim-line (line max-length)
  (let ((line-max (- max-length 3)))
    (if (> (length line) line-max)
	(format "%s..." (substring line 0 (- max-length 3)))
      line)))

(defun tmv--frame (lines)
  (when lines
    (let* ((max-line (apply #'max (mapcar #'length lines)))
	   (top (format "╭%s╮" (make-string max-line (aref "─" 0))))
	   (bottom (format "╰%s╯\n" (make-string max-line (aref "─" 0))))
	   (result-lines (list bottom)))
      (dolist (line (reverse lines))
	(let ((padding (make-string (- max-line (length line)) (aref " " 0))))
	  (push (format "│%s%s│" line padding) result-lines)))
      (string-join (cons top result-lines) "\n"))))

(defun tmv--frame-start-line? (line)
  (string-prefix-p "╭" (string-trim line)))

(defun tmv--frame-end-line? (line)
  (string-prefix-p "╰" (string-trim line)))

(defun tmv--within-frame? ()
  (let* ((line (string-trim (tmv--line))))
    (or (string-prefix-p "╭" line)
	(let ((within-frame? (or (string-prefix-p "│" line)
				 (string-prefix-p "╰" line))))
	  (when within-frame?
	    (let ((done? nil))
	      (save-excursion
		(forward-line -1)
		(while (not done?)
		  (cond ((bobp)
			 (setq done? t within-frame? nil))
			((string-prefix-p "╰" (string-trim (tmv--line)))
			 (setq done? t within-frame? nil))
			((string-prefix-p "╭" (string-trim (tmv--line)))
			 (setq done? t within-frame? t))
			(t (forward-line -1)))))
	      within-frame?))))))

(defun tmv--show-pane-tail ()
  (let ((id (tmuxmacs-view/id-at-point)))
    (when (equal (tmuxmacs-core/id-type id) :pane)
      (forward-line)
      (let* ((line-max (- (frame-width) 10)))
	(let ((inhibit-read-only t))
	  (insert
	   (tmv--frame
	    (mapcar
	     (lambda (line) (tmv--trim-line line line-max))
	     (split-string (tmuxmacs-pane/tail id) "\n")))))))))

(defun tmv--delete-frame ()
  (when (tmv--within-frame?)
    (let ((at-start? nil))
      (while (not at-start?)
	(if (tmv--frame-start-line? (tmv--line))
	    (setq at-start? t)
	  (forward-line -1))))
    (let ((at-end? nil))
      (while (not at-end?)
	(delete-line)
	(when (tmv--frame-end-line? (tmv--line))
	  (delete-line)
	  (setq at-end? t))))))

(defun tmv--pane-at-point? ()
  (equal (tmuxmacs-core/id-type (tmuxmacs-view/id-at-point)) :pane))

(defun tmv--hide-pane-tail ()
  (let ((inhibit-read-only t))
    (if (tmv--within-frame?)
	(tmv--delete-frame)
      (progn
	(forward-line)
	(tmv--delete-frame)))))

(defun tmuxmacs-view/goto-id (id)
  (goto-char (point-min))
  (let ((done? nil))
    (while (not done?)
      (if (or (eobp)
	      (equal (tmuxmacs-view/id-at-point) id))
	  (setq done? t)
	(forward-line)))))

(defun tmuxmacs-view/pane-tail-showing? ()
  (or (tmv--within-frame?)
      (and (tmv--pane-at-point?)
	      (save-excursion (progn (forward-line) (tmv--within-frame?))))))

(defun tmuxmacs-view/toggle-pane-tail ()
  (let ((id (tmuxmacs-view/id-at-point)))
    (when (equal (tmuxmacs-core/id-type id) :pane)
      (if (tmuxmacs-view/pane-tail-showing?)
	  (tmv--hide-pane-tail)
	(tmv--show-pane-tail))
      (tmuxmacs-view/goto-id id))))

(defun tmuxmacs-view/pane-tail-refresh ()
  (when (tmuxmacs-view/pane-tail-showing?)
    (tmuxmacs-view/toggle-pane-tail)
    (tmuxmacs-view/toggle-pane-tail)))


(provide 'tmuxmacs-view)
;;; tmuxmacs-view.el ends here
