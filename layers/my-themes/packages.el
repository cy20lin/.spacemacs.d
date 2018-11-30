;;; packages.el --- my-themes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <cy20lin@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-themes-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-themes/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-themes/pre-init-PACKAGE' and/or
;;   `my-themes/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-themes-packages
  `(
    (my-tango-dark-theme :location (recipe :fetcher file :path ,(concat (file-name-directory dotspacemacs-directory) "themes/" "my-tango-dark-theme/")))
    )
  "The list of Lisp packages required by the my-themes layer.")


(defun my-themes/init-my-tango-dark-theme ()
  (use-package my-tango-dark-theme))

;;; packages.el ends here
