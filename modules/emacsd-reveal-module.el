;;; emacsd-reveal-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reveal.js module
;; Enable ox-reveal for org-mode
;;
;; You must install git submodules to use this module:
;;  $ git submodule init && git submodule update


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

(let ((reveal-root-dir (concat emacsd-dir "reveal.js")))
  (when (file-exists-p reveal-root-dir)
    (require 'ox-reveal)
    (setq org-reveal-root (concat "file://" reveal-root-dir))))

(provide 'emacsd-reveal-module)

;;; emacsd-reveal-module.el ends here
