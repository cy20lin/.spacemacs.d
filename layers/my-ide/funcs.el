;;; funcs.el --- my-ide layer functions
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: ChienYu Lin <cy20lin@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; * Reference:
;;   - https://stackoverflow.com/questions/3569501/how-to-get-the-start-end-of-the-current-buffer-info-with-emacs-elisp
;;   - https://stackoverflow.com/questions/10594208/how-do-i-get-region-selection-programmably-in-emacs-lisp
;;   - http://ergoemacs.org/emacs/emacs_region.html

(defun spacemacs/my-ide-quickrun ()
  "select a quickrun implementation based on current evil-state"
  (interactive)
  (call-interactively
   (case evil-state
     ('replace 'quickrun-replace-region)
     ('visual 'quickrun-region)
     ('insert 'quickrun-replace-region)
     ('hybrid 'quickrun-replace-region)
     (otherwise 'quickrun))))

(defun spacemacs/my-ide-quickrun-replace-region ()
  "quickrun will replace all buffer region if current evil-state is 'normal"
  (interactive)
  (call-interactively
   (if (and (not (eq evil-state 'region)) (not (region-active-p)))
       (progn
         (goto-char (point-min))
         (push-mark (point-max))))
   (call-interactively 'quickrun-replace-region)))

;; hack to make buffer editable
(defun spacemacs//my-ide-quickrun-enable-edit ()
  "enable edit output buffer of quickrun"
  ;; (message "quickrun done")
  (with-current-buffer (get-buffer quickrun--buffer-name)
    (read-only-mode -1)))
