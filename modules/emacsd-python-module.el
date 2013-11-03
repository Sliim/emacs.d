;;; emacsd-python-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Python module
;; This module use elpy and rope python modules that must be installed on your system:
;;     pip install elpy rope ipython flake8 nose

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

(require 'elpy)

(elpy-enable)
(elpy-clean-modeline)
(setq elpy-rpc-backend "rope")

(add-hook 'python-mode-hook (lambda ()
                           (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

(provide 'emacsd-python-module)

;;; emacsd-python-module.el ends here
