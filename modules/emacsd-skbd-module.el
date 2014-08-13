;;; emacsd-skbd-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Emacs.d Skbd module

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

(require 'skbd)

(skbd-global-mode)

(defun emacsd-set-cmd-to-kbd (command keybinding)
  "Bind COMMAND to a KEYBINDING quickly."
  (interactive "sCommand: \nsKeybinding: ")
  (setq cmd command)
  (define-key skbd-mode-map (kbd keybinding) `(lambda ()
                                         (interactive)
                                         (let ((default-directory ,default-directory))
                                           (compile ,cmd)))))

(define-key skbd-mode-map (kbd "M-x") 'helm-M-x)
(define-key skbd-mode-map (kbd "M-<up>") 'windmove-up)
(define-key skbd-mode-map (kbd "M-<down>") 'windmove-down)
(define-key skbd-mode-map (kbd "M-<left>") 'windmove-left)
(define-key skbd-mode-map (kbd "M-<right>") 'windmove-right)
(define-key skbd-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key skbd-mode-map (kbd "C-+") 'text-scale-increase)
(define-key skbd-mode-map (kbd "C--") 'text-scale-decrease)
(define-key skbd-mode-map (kbd "C-x g") 'magit-status)
(define-key skbd-mode-map (kbd "C-c h") 'helm-custom)

(provide 'emacsd-skbd-module)

;;; emacsd-skbd-module.el ends here
