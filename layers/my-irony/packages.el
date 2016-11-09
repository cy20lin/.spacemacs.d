;;; packages.el --- my-irony layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <Vengis@VENGIS-PC>
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
;; added to `my-irony-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-irony/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-irony/pre-init-PACKAGE' and/or
;;   `my-irony/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-irony-packages
  '(irony
    company-irony
    flycheck-irony)
  "The list of Lisp packages required by the my-irony layer.

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
(setq irony-mode-excluded-packages
      '(auto-complete-clang))

(defun my-irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (message "[debug] my-irony/init-irony")
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)
      (add-hook 'irony-mode-hook
                (lambda ()
                  (define-key irony-mode-map [remap completion-at-point]
                    'irony-completion-at-point-async)
                  (define-key irony-mode-map [remap complete-symbol]
                    'irony-completion-at-point-async)))
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun my-irony/init-company-irony ()
  (use-package company-irony
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (progn
      (message "[debug] my-irony/init-company-irony")
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-irony))
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (add-hook 'irony-mode-hook 'company-mode))))

(defun my-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :if (configuration-layer/package-usedp 'flycheck)
    :defer t                            ; fix this ???
    :init
    (progn
      (message "[debug] my-irony/init-flycheck-irony")
      (eval-after-load 'flycheck
        '(add-to-list 'flycheck-checkers 'irony))
      (add-hook 'irony-mode-hook 'flycheck-mode))))

;;; packages.el ends here
