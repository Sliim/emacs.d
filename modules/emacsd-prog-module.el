;;; emacsd-prog-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Prog module

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

(require 'multiple-cursors)
(require 'restclient)

(defun emacsd-json-pretty-format ()
  "Print json string into an human readable format.
This function run external shell command `python -m json.tool` on current region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python -m json.tool"))

(defun emacsd-nxml-pretty-format()
  "Pretty print XML format. Requires xmllint in your path."
  (interactive)
  (save-excursion
    (shell-command-on-region (region-beginning) (region-end) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region begin end)))

(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace)
                            (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
                              (make-local-variable 'ac-sources)
                              (setq ac-sources '(ac-source-words-in-same-mode-buffers
                                                 ac-source-dictionary))
                              (when (and (require 'auto-complete-etags nil t) tags-table-list)
                                (add-to-list 'ac-sources 'ac-source-etags))
                              (auto-complete-mode t))))

(provide 'emacsd-prog-module)

;;; emacsd-prog-module.el ends here
