;;; emacsd-crux-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Crux module.
;; Enable some cool stuff from crux
;;     https://github.com/bbatsov/crux/

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

(require 'crux)
(require 'emacsd-skbd-module)

(define-key skbd-mode-map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(define-key skbd-mode-map (kbd "C-c b") 'crux-switch-to-previous-buffer)

(provide 'emacsd-crux-module)

;;; emacsd-crux-module.el ends here
