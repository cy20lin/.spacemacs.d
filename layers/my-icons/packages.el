;;; packages.el --- my-icons layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
;; added to `my-icons-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-icons/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-icons/pre-init-PACKAGE' and/or
;;   `my-icons/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-icons-packages
  '(
    all-the-icons
    all-the-icons-dired
    ;; all-the-icons-gnu
    ;; all-the-icons-ivy
    )
  "The list of Lisp packages required by the my-icons layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-icons/init-all-the-icons ()
  ""
  (use-package all-the-icons
    :init nil
    :config (when (configuration-layer/package-usedp 'neotree)
              ;; all-the-icons fonts are needed for neotree to display file icons properly
              ;; Download and install all fonts inside this repository: https://github.com/domtronn/all-the-icons.el/tree/master/fonts
              ;; (use-package all-the-icons)
              (setq neo-theme (if (display-graphic-p) 'icons 'ascii)))))

(defun my-icons/init-all-the-icons-dired ()
  ""
  :if (configuration-layer/package-usedp 'all-the-icons)
  (use-package all-the-icons
    :init nil
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    ))

;;; packages.el ends here
