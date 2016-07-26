;;; emacsd-mattermost-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides some functions to play with mattermost webhooks

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

(require 'url)
(require 'url-http)
(require 'json)

(defcustom matter-url ""
  "Mattermost webhook url."
  :type 'string)

(defcustom matter-icon-url "https://www.gnu.org/software/emacs/images/emacs.png"
  "Avatar icon url."
  :type 'string)

(defcustom matter-username "Emacs"
  "Username to display on mattermost."
  :type 'string)

(defun matter-send (msg url)
  "Send MSG to mattermost URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat "payload=" (url-hexify-string
                             (json-encode
                              (list :text msg
                                    :icon_url matter-icon-url
                                    :username matter-username))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (buffer-string))))

(defun matter-send-code (code lang title url)
  "Send CODE formatted with language LANG and TITLE to mattermost URL."
  (matter-send (concat "Pasted from emacs `" title "`:\n```" lang "\n" code "```\n") url))

(defun matter-region (lang)
  "Ask for language LANG and send current region to mattermost."
  (interactive "sLang: ")
  (if (and (region-beginning) (region-end))
      (matter-send-code (buffer-substring-no-properties (region-beginning) (region-end)) lang "selection" matter-url)
    "No selection"))

(defun matter-buffer (lang)
  "Ask for language LANG and send current buffer formatted as code to mattermost."
  (interactive "sLang: ")
  (matter-send-code (buffer-substring-no-properties (point-min) (point-max)) lang
                    (file-name-nondirectory buffer-file-name) matter-url))

(defun matter-message (message)
  "Send MESSAGE to mattermost."
  (interactive "sMessage: ")
  (matter-send message matter-url))

(defun matter-exec(command)
  "Execute a COMMAND and send output to mattermost."
  (interactive "sCommand: ")
  (matter-send (concat "Exec: `$ " command "`\n```\n" (substring (shell-command-to-string command) 0 -1) "\n```") matter-url))

(provide 'emacsd-mattermost-module)

;;; emacsd-mattermost-module.el ends here
