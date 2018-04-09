;;; funcs.el --- lsp Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//lsp-sync-peek-face ()
  "Synchronize the face used in `lsp-ui' peek window according to the theme."
  (set-face-attribute 'lsp-ui-peek-list nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-peek nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-filename nil
                      :foreground (face-attribute 'font-lock-constant-face
                                                  :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'highlight :foreground nil t)
                      :distant-foreground (face-attribute 'highlight
                                                          :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-header nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  )

(defun spacemacs/lsp-append-jump-handlers (mode)
    ;;; Override
  (let ((handler (intern (format "spacemacs-jump-handlers-%s" mode))))
    (add-to-list handler 'lsp-ui-peek-find-definitions))
  ;; The notion of 'spacemacs-reference-handlers' is the subject of this PR:
  ;; https://github.com/syl20bnr/spacemacs/pull/9911
  ;; Disabling for now...
  ;; (let ((handler (intern (format "spacemacs-reference-handlers-%s" mode))))
  ;;   (add-to-list handler 'lsp-ui-peek-find-references))
  )

(defun spacemacs/lsp-bind-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (spacemacs/declare-prefix-for-mode mode "m=" "format")
  (spacemacs/declare-prefix-for-mode mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode mode "ml" "lsp")
  (spacemacs/declare-prefix-for-mode mode "mr" "refactor")
  (spacemacs/declare-prefix-for-mode mode "mT" "toggle")

  (spacemacs/set-leader-keys-for-major-mode mode
    ;;Format
    "=b" #'spacemacs/lsp-format-buffer
    ;;goto
    "gi" #'lsp-ui-imenu
    "gd" #'lsp-ui-peek-find-definitions
    "gr" #'lsp-ui-peek-find-references
    "gs" #'lsp-ui-peek-find-workspace-symbol
    ;;refactor
    "rr" #'lsp-rename
    ;;toggles
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    "TF" #'spacemacs/lsp-ui-doc-func
    "TS" #'spacemacs/lsp-ui-sideline-symb
    "TI" #'spacemacs/lsp-ui-sideline-ignore-duplicate
    )

  )


(defun spacemacs/lsp-ui-doc-func ()
  "Toggle the function signature in the lsp-ui-doc overlay"
  (interactive)
  (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

(defun spacemacs/lsp-ui-sideline-symb ()
  "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
  (interactive)
  (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

(defun spacemacs/lsp-ui-sideline-ignore-duplicate ()
  "Toggle ignore duplicates for lsp-ui-sideline overlay"
  (interactive)
  (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate)))

;; Used for lsp-ui-peek-mode, but may be able to use some spacemacs fn. instead?
(defun spacemacs/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
      def (pop bindings))))
