;;; tmuxmacs-face.el -- jj colors -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: productivity
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 1.0.0

;;; Commentary:

;; Make some faces for tmux

;; Code:
(defconst tmuxmacs-face/session (list :foreground "#a6e3a1"))
(defconst tmuxmacs-face/window (list :foreground "#b4befe"))
(defconst tmuxmacs-face/pane (list :foreground "#fab387"))
(defconst tmuxmacs-face/id (list :foreground "#74c7ec"))
(defconst tmuxmacs-face/text (list :foreground "#cdd6f4"))
(defconst tmuxmacs-face/divider (list :foreground "#f9e2af"))
(defconst tmuxmacs-face/title (list :foreground "#cdd6f4" :weight 'bold))

(provide 'tmuxmacs-face)
;;; tmuxmacs-face.el ends here
