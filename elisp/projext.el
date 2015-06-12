;;; projext.el --- Extension for project managment utilities projectile/project-persist
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: projectile project-persist project

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Projext provide an extension for projectile and project-persist utilities

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

;; requires
(require 'project-persist)
(require 'projectile)

(defgroup projext nil
  "Extension for project managment utilities projectile/project-persist"
  :group 'projectile
  :group 'project-persist)

(defcustom projext-directory ".project/"
  "Directory where stored TAGS, desktop, snippets files."
  :group 'projext
  :type 'string)

(defcustom projext-config-file "config.el"
  "Specific configuration file for a project."
  :group 'projext
  :type 'string)

(defcustom projext-desktop-file "emacs.desktop"
  "Project's desktop filename."
  :group 'projext
  :type 'string)

(defcustom projext-tags-file "TAGS"
  "Project's tags file."
  :group 'projext
  :type 'string)

(defun projext-init ()
  "Projext initialization."
  (require 'project-persist)
  (project-persist-mode t)
  (projext-set-projectile-tags-command)

  ;; Loading project
  (add-hook 'project-persist-before-load-hook 'projext-close-if-opened)
  (add-hook 'project-persist-after-load-hook 'projext-hook-init)
  (add-hook 'project-persist-after-load-hook 'projext-hook-load-desktop)
  (add-hook 'project-persist-after-load-hook 'projext-hook-load-tags)
  (add-hook 'project-persist-after-load-hook 'projext-hook-load-snippets)
  (add-hook 'project-persist-after-load-hook 'projext-hook-load-config)
  (add-hook 'project-persist-after-load-hook 'projext-hook-dired-project-root)
  (add-hook 'project-persist-after-load-hook (lambda ()
                                               (remove-hook 'kill-emacs-hook 'project-persist--offer-save-if-open-project)))

  ;; Closing project
  (add-hook 'project-persist-before-close-hook 'projext-hook-clear-current-project-desktop)

  ;; Saving project
  (add-hook 'project-persist-after-save-hook 'projext-save-project-desktop)

  ;; Killing emacs
  (add-hook 'kill-emacs-hook 'projext-close-if-opened)

  ;; Additional settings
  (add-to-list 'project-persist-additional-settings '(languages . (lambda () (read-from-minibuffer "Comma-separated Project languages: ")))))


;; Interactive functions.
(defun projext-find ()
  "Find and load the given project name."
  (interactive)
  (project-persist--project-open (project-persist--read-project-name)))

(defun projext-show-current-project ()
  "Show the current project."
  (interactive)
  (if (project-persist--has-open-project)
      (message project-persist-current-project-name)
    (message "none")))

(defun projext-clear-project-desktop ()
  "Overload `desktop-clear` to open current project directory when clearing desktop."
  (interactive)
  (projext-setup-desktop-var)
  (desktop-clear)
  (when (project-persist--has-open-project)
    (dired project-persist-current-project-root-dir)))

(defun projext-save-project-desktop ()
  "Function that save current desktop in project's directory."
  (interactive)
  (if (project-persist--has-open-project)
      (progn
        (let ((p-dir (projext-get-project-directory)))
          (when (file-exists-p p-dir)
            (projext-setup-desktop-var)
            (desktop-save p-dir)
            (message "Desktop saved."))))
    (message "No project opened..")))

(defun projext-regenerate-tags ()
  "Regenerate project's tags table."
  (interactive)
  (projext-clean-project-tags)
  (projext-set-projectile-tags-command)
  (let ((p-tags (concat (projext-get-project-directory) projext-tags-file)))
    (if (project-persist--has-open-project)
        (progn
          (shell-command (format projectile-tags-command project-persist-current-project-root-dir))
          (visit-tags-table p-tags))
      (projectile-regenerate-tags))))

(defun projext-clean-project-desktop ()
  "Clear desktop and remove desktop files."
  (interactive)
  (when (project-persist--has-open-project)
    (let ((p-desk (concat (projext-get-project-directory) projext-desktop-file)))
      (projext-clear-project-desktop)
      (when (file-exists-p p-desk)
        (delete-file p-desk))
      (projext-remove-project-desktop-lock-file))))

(defun projext-clean-project-tags ()
  "Clear tags table and remove tags file."
  (interactive)
  (tags-reset-tags-tables)
  (when (project-persist--has-open-project)
    (let ((p-tags (concat (projext-get-project-directory) projext-tags-file)))
      (when (file-exists-p p-tags)
        (delete-file p-tags))))
  (when (file-exists-p (concat (projectile-project-root) "TAGS"))
    (delete-file (concat (projectile-project-root) "TAGS"))))

(defun projext-clean-project ()
  "Remove project's TAGS and desktop files."
  (interactive)
  (projext-clean-project-desktop)
  (projext-clean-project-tags))


;; project-persist hooks
(defun projext-hook-init ()
  "Project initialization when creating or opening project."
  (let ((p-dir (projext-get-project-directory)))
    (when (not (file-exists-p p-dir))
      (mkdir p-dir))))

(defun projext-hook-load-tags ()
  "Load project's TAGS file."
  (setq tags-completion-table nil)
  (let ((p-tags (concat (projext-get-project-directory) projext-tags-file)))
    (when (file-exists-p p-tags)
      (message "Loading project's tags table..")
      (tags-reset-tags-tables)
      (visit-tags-table p-tags))))

(defun projext-hook-load-desktop ()
  "Load project's desktop."
  (let ((p-desk (concat (projext-get-project-directory) projext-desktop-file)))
    (when (file-exists-p p-desk)
      (message "Loading project's desktop..")
      (projext-setup-desktop-var)
      (desktop-read))))

(defun projext-hook-load-snippets ()
  "Load project's snippets."
  (let ((p-snip (concat (projext-get-project-directory) "snippets/")))
    (when (file-exists-p p-snip)
      (message "Loading project's snippets..")
      (yas-load-directory p-snip))))

(defun projext-hook-load-config ()
  "Load project's configuration."
  (let ((p-conf (concat (projext-get-project-directory) projext-config-file)))
    (when (file-exists-p p-conf)
      (message "Loading project's configuration..")
      (load-file p-conf))))

(defun projext-hook-dired-project-root ()
  "Dired current project root directory."
  (dired project-persist-current-project-root-dir))

(defun projext-hook-clear-current-project-desktop ()
  "Clear current project desktop."
  (when (project-persist--has-open-project)
    (message "Clearing project desktop..")
    (projext-clear-project-desktop)
    (projext-remove-project-desktop-lock-file)))


;; Core functions.
(defun projext-setup-desktop-var ()
  "Setup desktop variable for current project."
  (setq desktop-path (list (projext-get-project-directory))
        desktop-dirname (projext-get-project-directory)
        desktop-base-file-name projext-desktop-file
        desktop-base-lock-name (concat projext-desktop-file ".lock")))

(defun projext-get-project-directory ()
  "Return project subdirectory where are stored TAGS file, desktop etc.."
  (concat project-persist-current-project-root-dir "/" projext-directory))

(defun projext-remove-project-desktop-lock-file ()
  "Remove desktop lock file."
  (when (project-persist--has-open-project)
    (let ((p-desktop-lock (concat (projext-get-project-directory) projext-desktop-file ".lock")))
      (when (file-exists-p p-desktop-lock)
        (delete-file p-desktop-lock)))))

(defun projext-close-if-opened ()
  "Close current project if opened."
  (when (project-persist--has-open-project)
    (project-persist-close)))

(defun projext-set-projectile-tags-command ()
  "Set projectile-tags-command custom variable."
  (setq p-base-command "ctags -Re \
    --exclude=\"\.git\" \
    --totals=yes \
    --tag-relative=yes \
    --PHP-kinds=-v \
    --regex-PHP='/abstract class ([^ ]*)/\1/c/' \
    --regex-PHP='/trait ([^ ]*)/\1/c/' \
    --regex-PHP='/interface ([^ ]*)/\1/c/' \
    --regex-PHP='/(public |final |static |abstract |protected |private )+function ([^ (]*)/\2/f/' \
    --regex-PHP='/const ([^ ]*)/\1/d/'")

  (when (project-persist--has-open-project)
    (when (project-persist--settings-get 'languages)
      (setq p-base-command (concat p-base-command " --languages=" (project-persist--settings-get 'languages))))
    (setq p-base-command (concat p-base-command " -o " (projext-get-project-directory) projext-tags-file)))

  (setq projectile-tags-command (concat p-base-command " %s")))

(provide 'projext)

;;; projext.el ends here
