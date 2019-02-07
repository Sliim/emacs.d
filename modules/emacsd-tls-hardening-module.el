;;; emacsd-tls-hardening-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d TLS hardening module
;; Source: https://www.reddit.com/r/emacs/comments/8sykl1/emacs_tls_defaults_are_downright_dangerous/

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

(require 'tls)

(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048
      gnutls-algorithm-priority "SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2"
      nsm-settings-file (expand-file-name "network-security.data" emacsd-var-dir)
      nsm-save-host-names t
      network-security-level 'high
      tls-checktrust t
      tls-program '("gnutls-cli -p %p --dh-bits=2048 --ocsp --x509cafile=%t --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM' %h"))

(provide 'emacsd-tls-hardening-module)

;;; emacsd-tls-hardening-module.el ends here
