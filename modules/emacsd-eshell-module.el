;;; emacsd-eshell-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Eshell module

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

(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/magit ()
  "Open magit in current repository."
  (interactive)
  (magit-status (eshell/pwd)))

(defun git-current-branch (pwd)
  "Return current git branch as a string in current directory `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
                          (if (> (length git-output) 0)
                              (concat (substring git-output 0 -1) (git-changes pwd))
                            "(no branch)")
                          "]") 'face `(:foreground "dim gray")))))

(defun git-unpushed-commits (pwd)
  "Return number of unpushed commits in repository `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git log @{u}.. --oneline 2>/dev/null | wc -l"))))
      (let ((out (substring git-output 0 -1)))
        (when (not (string= out "0"))
          (propertize (concat " [" out "]")
                      'face `(:foreground "red")))))))

(defun git-changes (pwd)
  "Get modified files in repository `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git status --short | wc -l"))))
      (let ((out (substring git-output 0 -1)))
        (when (not (string= out "0")) " ⚡")))))

(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
(setq eshell-highlight-prompt nil)
(setq eshell-history-size 512)
(setq eshell-directory-name (concat (getenv "HOME") "/.eshell"))
(setq eshell-prompt-regexp "^[^#$]*[#$] ")
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda ()
                        (replace-regexp-in-string
                         (concat "/home/" (getenv "USER")) "~"
                         (eshell/pwd)))) 'face `(:foreground "DodgerBlue2"))
         (or (git-current-branch (eshell/pwd)))
         (or (git-unpushed-commits (eshell/pwd)))
         (propertize " > " 'face `(:foreground "DodgerBlue2")))))

(provide 'emacsd-eshell-module)

;;; emacsd-eshell-module.el ends here
