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
(defun emacsd-init-packages()
  "Initialize packages directory."
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (setq package-user-dir emacsd-elpa-dir)
  (package-initialize))

(require 'guru-mode)
(add-hook 'prog-mode-hook 'guru-mode)

;;Global configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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
      inhibit-startup-screen t)

;; Setup Project-persist, projectile and projext
(require 'project-persist)
(setq project-persist-settings-dir (expand-file-name "project-persist" emacsd-var-dir))
(setq project-persist-auto-save-global nil)

(require 'projectile)
(require 'diminish)
(setq projectile-cache-file (expand-file-name "projectile.cache" emacsd-var-dir))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" emacsd-var-dir))
(projectile-global-mode t)
(diminish 'projectile-mode "Prjl")
(add-to-list 'projectile-globally-ignored-directories ".project")
(setq projectile-globally-ignored-files '())

(require 'projext)
(projext-init)

;; Mouse settings (disabling mouse-wheel and mouse keys)
(setq make-pointer-invisible t
      mouse-wheel-mode -1)
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; Ispell
(setq ispell-program-name "ispell")

;; Print config
(setq ps-font-size 8
      ps-header-font-size 9
      ps-header-title-font-size 10
      ps-line-number t
      ps-line-number-font-size 8
      ps-line-number-step 1
      ps-print-color-p (quote black-white))

;; Changelog config
(setq add-log-full-name "Sliim"
      add-log-mailing-address "sliim@mailoo.org")

;; Auto mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" emacsd-dir))

;;Small fix for selection with shift+up
; More infos: http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00174.html
(if (tty-type)
    (progn
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])
      (define-key input-decode-map "\e[1;5A" [C-up])
      (define-key input-decode-map "\e[1;5B" [C-down])
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])
      (define-key input-decode-map "\e[1;3A" [M-up])
      (define-key input-decode-map "\e[1;3B" [M-down])
      (define-key input-decode-map "\e[1;3C" [M-right])
      (define-key input-decode-map "\e[1;3D" [M-left])))

(provide 'emacsd-core)

;;; emacsd-core.el ends here
