;;; emacsd-notify-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d notifications module

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

(require 'notifications)

(defvar emacsd-notify-icon-theme-path (expand-file-name ".icons/ACYL_Icon_Theme_0.9.4/" (getenv "HOME"))
  "Icons theme path.")
(defvar emacsd-notify-icon "scalable/apps/emacs.svg"
  "Application icon path. Relative to icon theme path.")

(setq notifications-application-icon (expand-file-name emacsd-notify-icon emacsd-notify-icon-theme-path)
      notifications-on-action-map t
      notifications-on-action-object t
      notifications-on-close-map t
      notifications-on-close-object t)

(provide 'emacsd-notify-module)

;;; emacsd-notify-module.el ends here
