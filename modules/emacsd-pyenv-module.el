;;; emacsd-pyenv-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d pyenv module

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

(defvar pyenv-path "/home/sliim/.pyenv"
  "pyenv root directory.")

(when (file-exists-p pyenv-path)
  (let ((bin-path (concat pyenv-path "/bin"))
        (shims-path (concat pyenv-path "/shims")))
    (setenv "PATH"
            (concat bin-path ":" shims-path ":"
                    (getenv "PATH")))
    (setq python-check-command (concat shims-path "/flake8"))
    (setq elpy-rpc-python-command (concat shims-path "/python"))))

;;TODO: Implement some stuff

(provide 'emacsd-pyenv-module)

;;; emacsd-pyenv-module.el ends here
