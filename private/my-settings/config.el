;;; config.el --- Description

;; Copyright (C) 2016 Yu Yang

;;; Author: yangyu yangyu@ubuntu
;;; URL: https://github.com/yuyang0/
;;; Version: 0.1
;;; Package-Requires: ((package-name "version"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:

;; ELPA packages are available on Marmalade and Melpa. Alternatively, place
;; this file on a directory in your 'load-path', and explicitly require it.

;; Usage:
;;
;;; Code:

(setq-default
 user-full-name "Yu Yang"
 user-mail-address "yyangplus@NOSPAM.gmail.com")

;; for sdcv-mode
(require 'sdcv-mode)
(add-hook 'sdcv-mode-hook
          (lambda()
            (evil-define-key 'normal sdcv-mode-map "q" 'sdcv-return-from-sdcv)
            (evil-define-key 'normal sdcv-mode-map "d" 'sdcv-search)
            (evil-define-key 'normal sdcv-mode-map "?" 'describe-mode)
            (evil-define-key 'normal sdcv-mode-map "a" 'show-all)
            (evil-define-key 'normal sdcv-mode-map "h" 'hide-body)
            (evil-define-key 'normal sdcv-mode-map "e" 'show-entry)
            (evil-define-key 'normal sdcv-mode-map "c" 'hide-entry)
            ))
(global-set-key (kbd "C-c d") 'sdcv-search-no-prompt)
(spacemacs/set-leader-keys "o d" 'sdcv-search-no-prompt)
(spacemacs/set-leader-keys "o D" 'sdcv-search)

;; for artist mode

(defun artist-mode-toggle-emacs-state ()
  (if artist-mode
      (evil-emacs-state)
    (evil-exit-emacs-state)))

(unless (eq dotspacemacs-editing-style 'emacs)
  (add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state))

(add-to-list 'auto-mode-alist '("\\.artist\\'" . artist-mode))

;; for python
(require 'personal-utils)
(require 'personal-python)

(provide 'config)
;;; config.el ends here
