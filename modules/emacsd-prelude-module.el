
;;; emacsd-prelude-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude module.
;; Enable some cool stuff from
;;     https://github.com/bbatsov/prelude/

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

(require 'emacsd-skbd-module)

(defvar emacsd-prelude-dir (expand-file-name "prelude" emacsd-dir)
  "Prelude root directory.")
(defvar emacsd-prelude-modules-file (expand-file-name "prelude-modules.el" emacsd-dir)
  "Prelude modules file.")
(defvar emacsd-prelude-core-dir (expand-file-name "core" emacsd-prelude-dir)
  "Prelude core root directory.")
(defvar emacsd-prelude-modules-dir (expand-file-name "modules" emacsd-prelude-dir)
  "Prelude modules root directory.")

(defun emacsd-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (error (concat "Package " package " needed for prelude module."))))

(defun prelude-ensure-module-deps (packages)
  (mapc #'emacsd-require-package packages))

(defun emacsd-load-prelude ()
  (when (file-exists-p emacsd-prelude-core-dir)
    (load (expand-file-name "prelude-core.el" emacsd-prelude-core-dir)))
  (when (file-exists-p emacsd-prelude-modules-dir)
    (add-to-list 'load-path emacsd-prelude-modules-dir)
    (when (file-exists-p emacsd-prelude-modules-file)
      (load emacsd-prelude-modules-file))))

(setq prelude-flyspell t)
(setq prelude-guru t)
(setq prelude-whitespace nil)

(if (file-exists-p emacsd-prelude-dir)
    (emacsd-load-prelude)
  (message "Couldn't find prelude directory"))

;; Prelude core functions
(define-key skbd-mode-map (kbd "C-c d") 'prelude-duplicate-current-line-or-region)
(define-key skbd-mode-map (kbd "C-c b") 'prelude-switch-to-previous-buffer)

(provide 'emacsd-prelude-module)

;;; emacsd-prelude-module.el ends here
