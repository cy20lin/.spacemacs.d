;;; packages.el --- my-ide layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `my-ide-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-ide/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-ide/pre-init-PACKAGE' and/or
;;   `my-ide/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-ide-packages
  '(quickrun)
  "The list of Lisp packages required by the my-ide layer.

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


(defun my-ide/init-quickrun ()
  (use-package quickrun
    ;; :defer t
    :config
    (progn
      (spacemacs/declare-prefix "or" "run")
      (spacemacs/set-leader-keys
        ;; "orr" 'quickrun
        ;; "orr" 'quickrun-region
        "orr" 'spacemacs/my-ide-quickrun
        "orR" 'spacemacs/my-ide-quickrun-replace-region
        ;; "orR" 'quickrun-replace-region
        "ora" 'quickrun-with-arg
        "ors" 'quickrun-shell
        "orc" 'quickrun-compile-only
        "orh" 'helm-quickrun
        "orA" 'anything-quickrun
        "ore" 'quickrun-eval-print
        "ora" 'quickrun-autorun-mode)
      (setq quickrun-focus-p nil)
      ;; trying to enable edit on output buffer produce by quickrun,
      ;; sometimes it fails
      (add-hook 'quickrun-after-run-hook 'spacemacs//my-ide-quickrun-enable-edit)
      ;; (spacemacs/declare-prefix-for-mode 'emacs-lisp-mode "r" "run")
      ;; (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      ;;   "rr" 'quickrun
      ;;   "rR" 'quickrun-region
      ;;   "ra" 'quickrun-with-arg
      ;;   "rs" 'quickrun-shell
      ;;   "rc" 'quickrun-compile-only
      ;;   "rn" 'quickrun-replace-region
      ;;   "rh" 'helm-quickrun
      ;;   "rA" 'anything-quickrun
      ;;   "re" 'quickrun-eval-print
      ;;   "ra" 'quickrun-autorun-mode)
      )))
;; packages.el ends here
