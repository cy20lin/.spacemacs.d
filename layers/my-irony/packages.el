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
    (irony-eldoc
     :location (recipe
                :fetcher github
                :repo "ikirill/irony-eldoc"
                ;; Don’t spam irony if there were no candidates the first time.
                :commit "0df5831eaae264a25422b061eb2792aadde8b3f2"))
    (irony
     :location (recipe
                :fetcher github
                :repo "Sarcasm/irony-mode"
                ;; Make guess-flags choose the largest directory prefix
                :commit "aa74ed4d0e50202384526c705fc71b23088f42c9"))
    (company-irony
     :location (recipe
                :fetcher github
                :repo "Sarcasm/company-irony"
                ;; Release 1.1.0
                :commit "52aca45bcd0f2cb0648fcafa2bbb4f8ad4b2fee7"))
    (company-irony-c-headers
     :location (recipe
                :fetcher github
                :repo "hotpxl/company-irony-c-headers"
                ;; [master] Change readme.
                :commit "72c386aeb079fb261d9ec02e39211272f76bbd97"))
    (flycheck-irony
     :location (recipe
                :fetcher github
                ;; :repo "Sarcasm/flycheck-irony" ;; origin-repo
                :repo "cy20lin/flycheck-irony"
                ;; Use executable-find instead of file-exists-p
                :commit "1981712a7fefc8fc456e8499cdd1e49b01d7fbcf"))
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
      (with-eval-after-load 'irony
        (when (configuration-layer/package-used-p 'irony)
          (let* ((server-path (executable-find (expand-file-name "bin/irony-server" irony-server-install-prefix))))
            (if (not server-path)
                (let* ((exe (executable-find "irony-server"))
                       (exe-dir (directory-file-name (file-name-directory exe)))
                       (is-valid-prefix (string= "bin" (file-name-nondirectory exe-dir)))
                       (prefix (if is-valid-prefix (directory-file-name (file-name-directory exe-dir)))))
                  (if prefix (setq-default irony-server-install-prefix prefix))))))
        ;; (message "iorny-server-install-prefix : %S" irony-server-install-prefix)
        irony-server-install-prefix
        ))
    :config nil
    ))

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
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (add-hook 'irony-mode-hook 'company-mode)
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
