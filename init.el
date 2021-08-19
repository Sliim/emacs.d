;;; init.el --- Emacs.d.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs configuration

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

(when (version< emacs-version "24.1")
  (error "Requires at least GNU Emacs 24.1"))

(when (version< emacs-version "27")
  (package-initialize))

(defvar emacsd-dir (file-name-directory load-file-name)
  "The root dir of Emacs.")
(defvar emacsd-modules-dir (expand-file-name "modules" emacsd-dir)
  "Modules root directory.")
(defvar emacsd-core-dir (expand-file-name "core" emacsd-dir)
  "Core library root directory.")
(defvar emacsd-elisp-dir (expand-file-name "elisp" emacsd-dir)
  "My elisp code that I'm not packaged yet.")
(defvar emacsd-vendor-dir (expand-file-name "vendor" emacsd-dir)
  "Vendor's code not packaged.")
(defvar emacsd-var-dir (expand-file-name "var" emacsd-dir)
  "Var/ root directory.")
(defvar emacsd-themes-dir (expand-file-name "themes" emacsd-dir)
  "Themes root directory.")
(defvar emacsd-cask-dir (expand-file-name ".cask" emacsd-dir)
  "Cask root directory.")
(defvar emacsd-elpa-dir (expand-file-name
                         (concat (number-to-string emacs-major-version) "."
                                 (number-to-string emacs-minor-version) "/elpa")
                         emacsd-cask-dir)
  "Elpa packages root directory.")
(defvar emacsd-backup-dir (expand-file-name "backup" emacsd-var-dir)
  "This folder stores all the backup files.")
(unless (file-exists-p emacsd-backup-dir)
  (make-directory emacsd-backup-dir))

(defvar savefile-dir (expand-file-name "savefile" emacsd-var-dir)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(add-to-list 'custom-theme-load-path emacsd-themes-dir)
(add-to-list 'load-path emacsd-core-dir)
(add-to-list 'load-path emacsd-modules-dir)
(add-to-list 'load-path emacsd-elisp-dir)
(add-to-list 'load-path emacsd-vendor-dir)
(let ((default-directory emacsd-elpa-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Use core libraries
(require 'emacsd-core)
(require 'emacsd-editor)

;; Load modules
(load (expand-file-name "modules.el" emacsd-dir))

;; Set and load custom file
(setq custom-file (expand-file-name "custom.el" emacsd-dir))
(load custom-file)

;; Load private config if exists
(let ((user-config-file "~/.emacsrc"))
  (when (file-exists-p user-config-file)
    (load-file user-config-file)))

;;; init.el ends here
