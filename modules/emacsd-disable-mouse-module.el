;;; emacsd-disable-mouse-module.el --- Emacs.d modules.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: emacs.d modules

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Disable mouse settings (mouse-wheel and mouse keys)

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

(setq make-pointer-invisible t
      mouse-wheel-mode -1)
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(provide 'emacsd-disable-mouse-module)

;;; emacsd-disable-mouse-module.el ends here
