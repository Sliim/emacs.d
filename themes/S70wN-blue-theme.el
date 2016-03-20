;;; S70wN-blue-theme.el --- The S70wN theme

;; Author: Sliim <sliim@mailoo.org>
;; Keywords: faces
;; Compatibility: 24.4
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the S70wN theme
;;
;; To use this theme, download it to ~/.emacs.d/themes. In your `.emacs'
;; or `init.el', add this line:
;;
;;   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;
;; Once you have reloaded your configuration (`eval-buffer'), do `M-x
;; load-theme' and select "S70wN-blue".

;;; Code:

(deftheme S70wN-blue "The S70wN theme")
(let ((*background-color* "#102235")
      (*yellow* "yellow")
      (*comments* "#4E6F91")
      (*constant* "#FFC287")
      (*current-line* "#162C30")
      (*cursor-block* "#FFFFFF")
      (*cursor-underscore* "#FFFAAA")
      (*keywords* "#8AC6F2")
      (*light-purple* "#FFCCFF")
      (*line-number* "#2F577C")
      (*method-declaration* "#AF81F4")
      (*mode-line-bg* "#0A1721")
      (*mode-line-fg* "cyan")
      (*mode-line-inactive* "#4E6F91")
      (*normal* "#DFEFF6")
      (*number* "#96DEFA")
      (*operators* "#3E71A1")
      (*parens* "magenta")
      (*red* "#C62626")
      (*red-light* "#FFB6B0")
      (*regexp* "#EF7760")
      (*regexp-alternate* "#FF0")
      (*regexp-alternate-2* "#B18A3D")
      (*search-fg* "#E2DAEF")
      (*search-bg* "#AF81F4")
      (*string* "#89E14B")
      (*type* "#5BA0EB")
      (*variable* "#8AC6F2")
      (*vertical-border* "#0A1721")
      (*visual-selection* "#262D51")
      (*file* "#00BB00")
      (*black* "#000000"))

  (custom-theme-set-faces
   'S70wN-blue
   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background-color* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line*))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *red*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *type*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *red*))))

   ;; GUI
   `(fringe ((t (:background, *background-color*))))
   `(linum ((t (:background, *line-number*))))
   `(minibuffer-prompt ((t (:foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-line-bg* :foreground, *mode-line-inactive*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *vertical-border*))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *red* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *background-color* :foreground, *parens* :weight bold))))

   ;; search
   `(isearch ((t (:background, *search-bg* :foreground, *search-fg*))))
   `(isearch-fail ((t (:background, *red*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *search-fg*))))

   ;; magit
   `(magit-diff-add ((t (:foreground, *string*))))
   `(magit-diff-del ((t (:foreground, *red*))))

   ;; org-mode
   `(org-date ((t (:foreground, *light-purple* :underline t))))
   `(org-level-1 ((t (:foreground, *string*))))
   `(org-special-keyword ((t (:foreground, *variable*))))
   `(org-link ((t (:foreground, *keywords* :underline t))))
   `(org-checkbox ((t (:foreground, *keywords* :background, *background-color* :bold t))))
   `(org-clock-overlay ((t (:foreground, *mode-line-bg* :background, *string*))))

   ;; Helm
   `(helm-bookmark-directory ((t (:foreground, *type* :background, *variable* :bold t))))
   `(helm-bookmark-file ((t (:foreground, *file* :background, *variable*))))
   `(helm-bookmark-info ((t (:foreground, *string*))))
   `(helm-buffer-directory ((t (:foreground, *type* :bold t))))
   `(helm-buffer-process ((t (:foreground, *parens*))))
   `(helm-buffer-saved-out ((t (:foreground, *red* :background, *black*))))
   `(helm-buffer-size ((t (:foreground,  *number*))))
   `(helm-candidate-number ((t (:foreground, *number*))))
   `(helm-ff-directory ((t (:foreground, *type* :background, *black* :bold t))))
   `(helm-ff-excutable ((t (:foreground, *string* :background, *black* :bold t))))
   `(helm-ff-file ((t (:foreground, *keywords* :background, *black*))))
   `(helm-ff-invalid-symlink ((t (:foreground, *red-light* :background, *red*))))
   `(helm-ff-prefix ((t (:foreground, *regexp-alternate* :background, *regexp-alternate-2*))))
   `(helm-grep-cmd-line ((t (:foreground, *string* :background, *black* :bold t))))
   `(helm-grep-file ((t (:foreground, *file*))))
   `(helm-grep-finish ((t (:foreground, *string*))))
   `(helm-grep-lineno ((t (:foreground, *number*))))
   `(helm-grep-match ((t (:background, *black* :foreground, *yellow* :bold t))))
   `(helm-grep-running ((t (:foreground, *red* :background nil))))
   `(helm-header ((t (:foreground, *mode-line-fg* :background, *mode-line-bg*))))
   `(helm-lisp-show-completion ((t (:foreground, *black* :background, *string*))))
   `(helm-match ((t (:foreground, *file* :background, *black* :bold t))))
   `(helm-selection ((t (:foreground, *keywords* :background, *visual-selection*))))
   `(helm-selection-line ((t (:foreground nil :background, *yellow*))))
   `(helm-source-header ((t (:foreground, *mode-line-fg* :background, *mode-line-bg* :bold t))))

   ;; ERC
   `(erc-button ((t (:foreground, *vertical-border* :underline ,*vertical-border* :bold nil))))
   `(erc-current-nick-face ((t (:foreground, *keywords*))))
   `(erc-dangerous-hosts ((t (:foreground, *red* :bold t))))
   `(erc-direct-msg-face ((t (:foreground, *type*))))
   `(erc-error-face ((t (:foreground, *red*))))
   `(erc-header-face ((t (:background, *mode-line-bg*))))
   `(erc-input-face ((t (:foreground, *normal*))))
   `(erc-keyword-face ((t (:foreground, *keywords* :bold t))))
   `(erc-my-nick-face ((t (:foreground, *yellow* :bold t))))
   `(erc-nick-default-face ((t (:bold t :foreground, *type*))))
   `(erc-nick-msg-face ((t (:weight normal :foreground, *type*))))
   `(erc-notice-face ((t (:foreground, *normal*))))
   `(erc-pal-face ((t (:foreground, *parens*))))
   `(erc-prompt-face ((t (:bold t :foreground, *type* :background, *black*))))
   `(erc-timestamp-face ((t (:foreground, *regexp-alternate-2*))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'S70wN-blue)

;;; S70wN-blue-theme.el ends here
