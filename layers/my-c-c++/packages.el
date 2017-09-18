;; packages.el --- my-c-c++ layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <pclin@DESKTOP-A9NDFVV>
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
;; added to `my-c-c++-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-c-c++/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-c-c++/pre-init-PACKAGE' and/or
;;   `my-c-c++/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-c-c++-packages
  `(
    (my-c-c++ :location local)
    projectile
    irony
    aide
    )
  "The list of Lisp packages required by the my-c-c++ layer.

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

(defun my-c-c++/init-my-c-c++ ()
  (use-package my-c-c++
    :init nil
    :config nil))

(defun my-c-c++/pre-init-projectile ()
  ;; (message "pre-init-projectile configs for cmake")
  (spacemacs|use-package-add-hook projectile
    :pre-config
    (progn
      (add-to-list 'projectile-project-root-files-top-down-recurring "CMakeLists.txt"))))

(defun my-c-c++/pre-init-aide ()
  ;; (message "pre-init-aide")
  (spacemacs|use-package-add-hook aide
    :pre-config
    (aide-register-project-type
     'cmake '("CMakeLists.txt")
     :compile "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -H. -Bbuild && cmake --build build --target all --config Release"
     :test "cmake --build build --target test"
     :run "cmake --build build --target run"
     )))

(defun my-c-c++/pre-init-irony ()
  (spacemacs|use-package-add-hook irony
    :pre-config
    (progn
      (defun my-c-c++//enable-irony-mode-if-server-found ()
        (let* ((exec-path (cons (expand-file-name "bin" irony-server-install-prefix) exec-path)))
          (if (executable-find "irony-server") (irony-mode))))
      (add-hook 'c-mode-hook 'my-c-c++//enable-irony-mode-if-server-found)
      (add-hook 'c++-mode-hook 'my-c-c++//enable-irony-mode-if-server-found)
      (with-eval-after-load "irony-cdb"
        (add-to-list 'irony-cdb-compilation-databases 'my-c-c++-cdb-guess t))
      )))

;;; packages.el ends here
