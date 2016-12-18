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
  '(company-irony
    company-irony-c-headers
    irony
    irony-eldoc
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

;; (setq irony-mode-excluded-packages
;;       '(auto-complete-clang))
(defun my-irony-install-server ()
  ""
  (let* ((build-dir (or irony-server-build-dir
                        (concat
                         (file-name-as-directory temporary-file-directory)
                         (file-name-as-directory
                          (format "build-irony-server-%s" (irony-version))))))
         (command (format
                   (concat "%s -H%s -B%s -G%s %s && %s --build %s"
                           " --use-stderr --config Release --target install")
                   (shell-quote-argument irony-cmake-executable)
                   (shell-quote-argument irony-server-source-dir)
                   (shell-quote-argument build-dir)
                   (shell-quote-argument "Unix Makefiles")
                   (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                                 (expand-file-name
                                                  irony-server-install-prefix)))
                   (shell-quote-argument irony-cmake-executable)
                   (shell-quote-argument build-dir)
                   )))
    (if (file-exists-p build-dir)
        (progn
          (delete-directory build-dir t)
          (make-directory build-dir t)))
    (irony-install-server command)))


(defun my-irony-cdb-default--get-compile-options ()
  (flags (my-cxx--guess-flags (buffer-file-name) major-mode)))

(defun my-irony-cdb-default (command &rest args)
  (cl-case command
    (get-compile-options (my-irony-cdb-default--get-compile-options))))

(defun my-irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode))
    :config
    (progn
      ;; (setq irony-user-dir (f-slash (f-join user-home-directory "bin" "irony")))
      ;; (setq irony-server-intsall-prefix irony-user-dir)
      ;; current solution: install server, when irony is first loaded.
      ;; is there a way to install server just right after `irony' is installed? 
      (if (not (irony--locate-server-executable)) (my-irony-install-server))
      (spacemacs|diminish irony-mode " Ⓘ" " I")
      (defun my-irony/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
        (setq irony-cdb-compilation-databases (cons 'my-irony-cdb-default irony-cdb-compilation-databases)))
      (add-hook 'irony-mode-hook 'my-irony/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))

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
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))

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
