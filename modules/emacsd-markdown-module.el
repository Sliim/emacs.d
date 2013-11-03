;;; emacsd-markdown-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Git-Messenger module

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

(add-hook 'markdown-mode-hook (lambda ()
                                (make-local-variable 'before-save-hook)
                                (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
                                (define-key markdown-mode-map (kbd "C-c C-c p") 'markdown-preview-with-hf)))

(setq markdown-xhtml-header-content "<meta charset='utf-8'>")

(defun markdown-preview-with-hf (&optional output-buffer-name)
  "Run `markdown' on the current buffer and preview the output 'OUTPUT-BUFFER-NAME' in a browser."
  (interactive)
  (browse-url-of-buffer (markdown-standalone markdown-output-buffer-name)))

(provide 'emacsd-markdown-module)

;;; emacsd-markdown-module.el ends here
