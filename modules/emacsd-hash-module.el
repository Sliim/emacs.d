;;; emacsd-hash-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Hash module

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

(defun md5-hash-region (start end)
  "Print md5 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'md5 (current-buffer) start end)))

(defun sha1-hash-region (start end)
  "Print sha1 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'sha1 (current-buffer) start end)))

(defun sha224-hash-region (start end)
  "Print sha224 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'sha224 (current-buffer) start end)))

(defun sha256-hash-region (start end)
  "Print sha256 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'sha256 (current-buffer) start end)))

(defun sha384-hash-region (start end)
  "Print sha384 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'sha384 (current-buffer) start end)))

(defun sha512-hash-region (start end)
  "Print sha512 hash from START to END of the current region."
  (interactive "r")
  (message (secure-hash 'sha512 (current-buffer) start end)))

(defun md5-hash-string (str)
  "Print md5 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'md5 str)))

(defun sha1-hash-string (str)
  "Print sha1 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'sha1 str)))

(defun sha224-hash-string (str)
  "Print sha224 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'sha224 str)))

(defun sha256-hash-string (str)
  "Print sha256 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'sha256 str)))

(defun sha384-hash-string (str)
  "Print sha384 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'sha384 str)))

(defun sha512-hash-string (str)
  "Print sha512 hash for given STR."
  (interactive "sString: ")
  (message (secure-hash 'sha512 str)))

(defun md5-hash-buffer ()
  "Print md5 hash of the current buffer."
  (interactive)
  (message (secure-hash 'md5 (current-buffer))))

(defun sha1-hash-buffer ()
  "Print sha1 hash of the current buffer."
  (interactive)
  (message (secure-hash 'sha1 (current-buffer))))

(defun sha224-hash-buffer ()
  "Print sha224 hash of the current buffer."
  (interactive)
  (message (secure-hash 'sha224 (current-buffer))))

(defun sha256-hash-buffer ()
  "Print sha256 hash of the current buffer."
  (interactive)
  (message (secure-hash 'sha256 (current-buffer))))

(defun sha384-hash-buffer ()
  "Print sha384 hash of the current buffer."
  (interactive)
  (message (secure-hash 'sha384 (current-buffer))))

(defun sha512-hash-buffer ()
  "Print sha512 hash of the current buffer."
  (interactive)
  (message (secure-hash 'sha512 (current-buffer))))

(provide 'emacsd-hash-module)

;;; emacsd-hash-module.el ends here
