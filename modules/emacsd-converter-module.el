;;; emacsd-converter-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Converter module

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

(require 'hex-util)
(require 'url-util)

(defun caesar-encode (text key)
  "Encode TEXT as caesar cypher with given shift KEY."
  (with-temp-buffer
    (dotimes (i (length text))
      (let ((oct (aref text i))
            (max nil)
            (min nil)
            (new_oct nil))
        (when (and (< oct 58) (> oct 47))
          (setf min 47)
          (setf max 57))
        (when (and (< oct 123) (> oct 96))
          (setf min 96)
          (setf max 122))
        (when (and (< oct 91) (> oct 64))
          (setf min 64)
          (setf max 90))
        (if (or (not min) (not max))
            (setf new_oct oct)
          (progn
            (setf new_oct (+ oct (string-to-number key)))
            (when (> new_oct max)
              (setf new_oct (+ (mod new_oct max) min)))))
        (insert new_oct)))
    (buffer-string)))

(defun caesar-decode (text key)
  "Decode TEXT as caesar cypher with given shift KEY."
  (with-temp-buffer
    (dotimes (i (length text))
      (let ((oct (aref text i))
            (max nil)
            (min nil)
            (new_oct nil))
        (when (and (< oct 58) (> oct 47))
          (setf min 48)
          (setf max 58))
        (when (and (< oct 123) (> oct 96))
          (setf min 97)
          (setf max 123))
        (when (and (< oct 91) (> oct 64))
          (setf min 65)
          (setf max 91))
        (if (or (not min) (not max))
            (setf new_oct oct)
          (progn
            (setf new_oct (- oct (string-to-number key)))
            (when (< new_oct min)
              (setf new_oct (- max (mod min new_oct))))))
        (insert new_oct)))
    (buffer-string)))

(defun binary-encode (text)
  "Encode a TEXT in binary string."
  (with-temp-buffer
    (dotimes (i (length text))
      (let ((res "")
            (j (aref text i)))
        (while (not (= j 0))
          (setf res (concat (if (= 1 (logand j 1)) "1" "0") res))
          (setf j (lsh j -1)))
        (if (string= res "")
            (setf res "0"))
        (insert res))
      (insert " "))
    (buffer-string)))

(defun binary-decode (text)
  "Decode a TEXT in binary string."
  (with-temp-buffer
    (dolist (elt (split-string text))
      (let ((res 0)
            (i 1)
            (revelt (reverse elt)))
        (dotimes (j (length revelt))
          (when (string= (substring revelt j (+ j 1)) "1")
            (setf res (+ res i)))
          (setf i (* i 2)))
        (insert res)))
    (buffer-string)))

(defun base64-encode-string (str)
  "Base64 STR encode."
  (interactive "sString: ")
  (message "%S" (base64-encode-string str)))

(defun base64-decode-string (str)
  "Base64 STR decode."
  (interactive "sString: ")
  (message "%S" (base64-decode-string str)))

(defun hex-encode-string (str)
  "Encode STR to hexadecimal."
  (interactive "sString: ")
  (message "%S" (encode-hex-string str)))

(defun hex-encode-region (start end)
  "Encode string from START to END to hexadecimal."
  (interactive "r")
  (let ((res (encode-hex-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(defun hex-decode-string (str)
  "Decode STR to hexadecimal."
  (interactive "sString: ")
  (message "%S" (decode-hex-string str)))

(defun hex-decode-region (start end)
  "Decode string from START to END to hexadecimal."
  (interactive "r")
  (let ((res (decode-hex-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(defun url-encode-string (str)
  "Encode STR Url."
  (interactive "sURL: ")
  (message "%S" (url-hexify-string str)))

(defun url-encode-region (start end)
  "Encode Url from START to END."
  (interactive "r")
  (let ((res (url-hexify-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(defun url-decode-string (str)
  "Decode STR Url."
  (interactive "sURL: ")
  (message "%S" (url-unhex-string str)))

(defun url-decode-region (start end)
  "Decode Url from START to END."
  (interactive "r")
  (let ((res (url-unhex-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(defun caesar-encode-string (text key)
  "Encode TEXT as caesar cypher with given KEY."
  (interactive "sText: \nsKey: ")
  (message (caesar-encode text key)))

(defun caesar-decode-string (text key)
  "Decode TEXT as caesar cypher with given KEY."
  (interactive "sText: \nsKey: ")
  (message (caesar-decode text key)))

(defun caesar-encode-region (start end key)
  "Encode region as caesar cypherfrom START to END with given KEY."
  (interactive "r\nsKey: ")
  (let ((res (caesar-encode (buffer-substring-no-properties start end) key)))
    (delete-region start end)
    (insert res)))

(defun caesar-decode-region (start end key)
  "Decode region as caesar cypherfrom START to END with given KEY."
  (interactive "r\nsKey: ")
  (let ((res (caesar-decode (buffer-substring-no-properties start end) key)))
    (delete-region start end)
    (insert res)))

(defun binary-encode-string (text)
  "Encode TEXT to binary string."
  (interactive "sText: ")
  (message (binary-encode text)))

(defun binary-decode-string (text)
  "Decode TEXT from binary string."
  (interactive "sString: ")
  (message (binary-decode text)))

(defun binary-encode-region (start end)
  "Encode TEXT to binary string from START to END in region."
  (interactive "r")
  (let ((res (binary-encode (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(defun binary-decode-region (start end)
  "Decode TEXT from binary string from START to END in region."
  (interactive "r")
  (let ((res (binary-decode (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert res)))

(provide 'emacsd-converter-module)

;;; emacsd-converter-module.el ends here
