;;; emacsd-helm-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d helm module

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

(require 'helm)
(require 'helm-aliases)
(require 'helm-misc)
(require 'helm-files)
(require 'helm-ls-git)
(require 'helm-etags+)
(require 'helm-project-persist)
(require 'helm-projectile)
(require 'helm-swoop)
(require 'helm-git-grep)
(require 'helm-c-yasnippet)
(require 'helm-pydoc)

(setq helm-buffers-favorite-modes (quote (emacs-lisp-mode org-mode php-mode ruby-mode python-mode shell-script-mode))
      helm-follow-mode-persistent t
      helm-etags+-use-short-file-name 'absolute)

(require 'helm-github-stars)
(setq helm-github-stars-username "Sliim")
(setq helm-github-stars-cache-file (expand-file-name "hgs-cache" emacsd-var-dir))

(defun emacsd-helm-buffers-right-side ()
  "Special helm settings to list buffers in right side."
  (interactive)
  (let ((initial-helm-split-window-default-side helm-split-window-default-side))
    (setq helm-split-window-default-side (quote right))
    (helm-buffers-list)
    (setq helm-split-window-default-side initial-helm-split-window-default-side)))

(defun helm-custom ()
  "Custom helm interface."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                         helm-source-recentf
                         helm-source-ls-git-status
                         helm-source-ls-git
                         helm-source-buffer-not-found)
                       "*helm custom*")))


(provide 'emacsd-helm-module)

;;; emacsd-helm-module.el ends here
