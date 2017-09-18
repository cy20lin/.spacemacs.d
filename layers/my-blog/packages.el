;;; packages.el --- my-blog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: ChienYu Lin <cy20lin@gmail.com>
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
;; added to `my-blog-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-blog/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-blog/pre-init-PACKAGE' and/or
;;   `my-blog/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-blog-packages
  '((blog-admin :location (recipe
                           :fetcher github
                           :repo "CodeFalling/blog-admin"))
    aide
    )
  "The list of Lisp packages required by the my-blog layer.

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

(defun my-blog/init-blog-admin ()
  (use-package blog-admin
    :init
    (progn
      ;; NOTE: Maybe create alias variables or set variables in config.el
      ;; (setq blog-admin-backend-type 'hexo)
      ;; (setq blog-admin-backend-new-post-in-drafts t)
      ;; (setq blog-admin-backend-new-post-with-same-name-dir t)
      ;; (setq blog-admin-backend-path "~/blog")
      )
    :config
    (progn
      (add-hook 'blog-admin-mode-hook 'my-blog//blog-admin-mode-init)
      ;; (add-to-list 'evil-motion-state-modes 'blog-admin-mode)
      ;; (evilified-state-evilify-map ...)
      ;; (add-to-list 'evil-buffer-regexps '("\\*Flycheck")).
      ;; (push '("^\\*Blog\\*" . evilified) evil-buffer-regexps)
      (add-to-list 'evil-buffer-regexps '("^\\*Blog\\*$" . evilified))
      ;; (kbd dotspacemacs-leader-key)
      ;; (with-eval-after-load 'magit
      ;; (define-key magit-status-mode-map
      ;;   (kbd dotspacemacs-leader-key) spacemacs-default-map))
      )))

(defun my-blog/pre-init-aide ()
  (spacemacs|use-package-add-hook aide
    :pre-config
    (progn
      (when (configuration-layer/package-usedp 'blog-admin)
        (spacemacs/declare-prefix "ma" "applications")
        ;; NOTE: It would be great if we could use
        ;; (spacemacs/set-leader-keys "mab" 'blog-admin-start)
        (spacemacs/set-leader-keys-for-minor-mode 'aide-mode
          "ab" 'blog-admin-start)
        ))))

;;; packages.el ends here
