;;; emacsd-core.el --- Emacs.d core library.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs.d core library

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir emacsd-elpa-dir)
(package-initialize)

(require 'guru-mode)
(add-hook 'prog-mode-hook 'guru-mode)

;;Global configuration
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(menu-bar-mode -1)
;; (blink-cursor-mode -1)
(delete-selection-mode t)
(global-auto-revert-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p
      require-final-newline t
      c-basic-offset 4
      scroll-conservatively 10000
      scroll-step 1
      delete-trailing-lines t
      vc-follow-symlinks t
      inhibit-startup-screen t
      ffap-machine-p-known 'reject)

;; Src: https://stackoverflow.com/questions/8412144/c-a-to-go-\
;; to-the-first-character-in-emacs-using-ipython-mode
(defun beginning-or-indent ()
  "Go to beginning or indentation."
  (interactive)
  (if (eq (current-column) 0)
      (back-to-indentation)
    (move-beginning-of-line 1)))
(global-set-key (kbd "C-a") 'beginning-or-indent)

;; Ispell
(setq ispell-program-name "ispell")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(provide 'emacsd-core)

;;; emacsd-core.el ends here
