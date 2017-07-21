;;; funcs.el --- my-ide layer packages file for Spacemacs.
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
  (message "run enable edit hook")
  (with-current-buffer (get-buffer quickrun--buffer-name)
    (read-only-mode -1)))


;; quickrun commands
(quickrun-add-command "javascript/wscript"
  '((:command . "wscript")
    (:exec . "%c //e:jscript %o %s %a")
    (:cmdopt . "//Nologo")
    (:description . "Run Javascript file with wscript"))
  :mode 'javascript-mode)

(quickrun-add-command "vbscript/wscript"
  '((:command . "wscript")
    (:exec . "%c //e:vbscript %o %s %a")
    (:cmdopt . "//Nologo")
    (:description . "Run Vbscript file with wscript"))
  :mode 'visual-basic-mode)

(quickrun-add-command "vbscript/cscript"
  '((:command . "cscript")
    (:exec . "%c //e:vbscript %o %s %a")
    (:cmdopt . "//Nologo")
    (:description . "Run Vbscript file with cscript"))
  :devault "vbscript"
  :mode 'visual-basic-mode)

(quickrun-add-command "cmd"
  '((:command . "cmd")
    (:exec . "%c //e %s %a")
    (:description . "Run batch file with cmd"))
  :default "cmd"
  :mode 'dos-mode)

(quickrun-add-command "bat"
  '((:command . "cmd")
    (:exec . "%c //c %s %a")
    (:description . "Run batch file with cmd"))
  :default "bat"
  :mode 'dos-mode)

(quickrun-add-command "lisp/ecl"
  '((:command . "ecl")
    (:exec . "%c --shell %s %a")
    (:description . "Run Lisp file with ecl"))
  :default "lisp"
  :mode 'lisp-mode)

(quickrun-add-command "cmake"
  '((:command . "cmake")
    (:exec . "%c %o -P %s %a")
    (:description . "Run CMake script"))
  :default "cmake"
  :mode 'cmake-mode)
