;;; packages.el --- my-settings Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-settings-packages
    '(
      url-shortener
      yatemplate
      ;; package names go here
      ))

;; List of packages to exclude.
(setq my-settings-excluded-packages '())

;; For each package, define a function my-settings/init-<package-name>
;;
(defun my-settings/init-url-shortener()
  "Initialize my package"
  (use-package url-shortener)
  )
(defun my-settings/init-yatemplate()
  "Initialize my package"
  (use-package yatemplate
    :init
    (auto-insert-mode +1)
    :config
    (progn
      (setq yatemplate-dir (locate-user-emacs-file "private/my-settings/yatemplates"))
      (yatemplate-fill-alist)))
  )
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
