;;; emacsd-emms-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Emms module

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

(require 'emms-setup)
(require 'emms-streams)
(require 'emms-info)
(require 'emms-info-mp3info)
(require 'emms-browser)

(defvar emacsd-emms-dir (expand-file-name "emms" emacsd-var-dir)
  "Directory for emms cache, history, etc.. files.")

(emms-standard)
(emms-default-players)

(setq emms-source-file-default-directory "~/musics/")
(setq emms-score-file (expand-file-name "scores" emacsd-emms-dir))
(setq emms-stream-bookmarks-file (expand-file-name "streams" emacsd-emms-dir))
(setq emms-history-file (expand-file-name "history" emacsd-emms-dir))
(setq emms-cache-file (expand-file-name "cache" emacsd-emms-dir))

(add-to-list 'emms-info-functions 'emms-info-mp3info)


(provide 'emacsd-emms-module)

;;; emacsd-emms-module.el ends here
