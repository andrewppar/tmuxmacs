;;; tmux-interface.el --- Manage tmux within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: tmux
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Elisp interface for running tmux commands

;;; Code:
(require 'cl-macs)

(defun tmux-command-output (&rest args)
  "Run a tmux command with ARGS."
  (let* ((temp-buffer-name "tmux-buffer")
	 (arg-list `("tmux" nil ,temp-buffer-name nil ,@args))
	 (result ""))
    (save-window-excursion
      (with-output-to-temp-buffer temp-buffer-name
   	(switch-to-buffer temp-buffer-name)
	(apply #'call-process arg-list)
	(setq result (buffer-substring (point-min) (point-max)))))
    (kill-buffer temp-buffer-name)
    result))

(defun tmux-command-output-lines (&rest args)
  "Run tmux command ARGS and return the output as a list of lines."
  (let ((result '()))
    (dolist (s (split-string (apply #'tmux-command-output args) "\n"))
      (unless (equal s "")
	(push s result)))
    (reverse result)))

(defun tmux-command->alist (&rest args)
  "Create an alist from ARGS.

Assumes that the command returns two columns separated by whitespace."
  (mapcar
   (lambda (line)
     (cl-destructuring-bind (key value)
	 (split-string line)
       (cons key value)))
   (apply #'tmux-command-output-lines args)))

(provide 'tmux-interface)
;;; tmux-interface.el ends here
