;;; packages.el --- my-irony layer packages file for Spacemacs.
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

;; Install and configure `irony' itself and related packages
;;

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
  '(
    ;; irony-eldoc
    (irony :location (recipe :fetcher github :repo Sarcasm/irony-mode))
    (company-irony :location (recipe :fetcher github :repo Sarcasm/company-irony))
    company-irony-c-headers
    flycheck-irony
    )
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

(defun my-irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (spacemacs|diminish irony-mode " Ⓘ" " I")
      (defun my-irony/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      )
    :config nil))

(defun my-irony/init-company-irony ()
  (use-package company-irony
    :if
    (and
     (configuration-layer/layer-usedp 'auto-completion)
     (configuration-layer/package-usedp 'company))
    :defer t
    :commands company-irony
    :init
    (progn
      ;; (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
      ;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      ;; (add-hook 'irony-mode-hook 'company-mode)
      (push 'company-irony company-backends-c-mode-common)
      (spacemacs|use-package-add-hook irony
        :post-config (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))
    :config nil))

(defun my-irony/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :if
    (and
     (configuration-layer/layer-usedp 'auto-completion)
     (configuration-layer/package-usedp 'company)
     )
    :defer t
    :commands company-irony-c-headers
    :init
    (progn
      (push 'company-irony-c-headers company-backends-c-mode-common))))

(defun my-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :if
    (and
     (configuration-layer/layer-usedp 'syntax-checking)
     (configuration-layer/package-usedp 'flycheck))
    :defer t
    :init
    (progn
      (add-hook 'irony-mode-hook 'flycheck-irony-setup)
      (eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'irony))
      (add-hook 'irony-mode-hook 'flycheck-mode)
      )))

(defun my-irony/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))
;;; packages.el ends here
