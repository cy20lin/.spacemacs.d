;;; msystem.el --- Teach EMACS about msystem styles and mount points.

;; Copyright (C) 2018 ChienYu Lin <cy20lin@gmail.com>.

;; Author: ChienYu Lin <cy20lin@gmail.com>
;; Keywords: msys2, msys, msystem, cygwin

(defun msystem--p ()
  "Resolve if is in msystem environment."
  (getenv "MSYSTEM"))

(defvar msystem-p (msystem--p)
  "Check is in msystem environment.")

(defun msystem-p ()
  "Check is in msystem environment."
  msystem-p)

(defun msystem--find-root ()
  "Find msystem root on windows"
  (let* ((str (shell-command-to-string "cygpath --mixed /"))
         (beg (string-match "^[a-zA-Z]:\\(?:/[^/:|\n]*\\)*$" str))
         (end (match-end 0)))
    (if beg (substring str beg end))))

(defvar msystem-root (msystem--find-root)
  "Msystem root on windows.")

(defun msystem-root ()
  "Msystem root on windows."
  msystem-root)

(defun msystem--transform-path (&optional path default-directory &rest dummies)
  "transform path if is msystem style path"
  (let* ((len (length path))
         (a0 (if (> len 0) (substring path 0 1)))
         (a1 (if (> len 1) (substring path 1 2)))
         (a2 (if (> len 2) (substring path 2 3))))
    (cond
     ;; not start with "/"
     ((or (null a0) (not (string-equal "/" a0))) nil)
     ;; start with "/", but second char is not alphabetical
     ((or (null a1) (string-match "^[^a-zA-Z]$" a1)) (concat (directory-file-name (msystem-root)) path))
     ;; start with "/", second char is alphabetical, third char is nil
     ((null a2) (concat a1 ":/"))
     ;; start with "/", second char is alphabetical, third char is "/"
     ((string-match a2 "/") (concat a1 ":" (substring path 2)))
     ;; otherwise
     (t (concat (directory-file-name (msystem-root)) path))
     )
    ))

(defun msystem--advice-expand-file-name (oldfun name &optional dir &rest dummies)
  "return advised path in msystem, suggest using :around in add-function"
  ;; example usage:
  ;; (msystem--advice-expand-file-path (symbol-function 'expand-file-name) "/" "..")
  (let ((name- (msystem--transform-path name))
        (dir- (msystem--transform-path dir)))
    (apply oldfun (list* (or name- name) (or dir- dir) dummies))))

(defun msystem-setup-advice ()
  (if (msystem-p)
      (add-function :around (symbol-function 'expand-file-name) 'msystem--advice-expand-file-name)))

(defun msystem-teardown-advice ()
  (if (msystem-p)
      (remove-function (symbol-function 'expand-file-name) 'msystem--advice-expand-file-name)))

(defun msystem-eshell-alternate-command (command)
  (let* ((cmd (msystem--transform-path command))
         (exe (and cmd (executable-find cmd))))
    (if (or (not exe)
            (string-match-p "^/bin" command)
            (string-match-p "^/local/bin" command))
        (progn
          (setq cmd (msystem--transform-path (concat "/usr" command)))
          (setq exe (and cmd (executable-find cmd)))))
    exe))

;; (defun msystem-eshell-named-command (command arguments)
;;   (let ((cmd (msystem-eshell-alternate-command command)))
;;     (if cmd (eshell-plain-command cmd arguments))))

(defun msystem-setup-eshell ()
  (when (require 'eshell nil 'noerror)
    ;; hooks:
    ;; (add-to-list 'eshell-prepare-command-hook )
    ;; (add-to-list 'eshell-named-command-hook 'msystem-eshell-named-command)
    (add-to-list 'eshell-alternate-command-hook 'msystem-eshell-alternate-command)
    ))

(defun msystem-teardown-eshell ()
  (when (require 'eshell nil 'noerror)
    (setq eshell-named-command-hook (remove 'msystem-eshell-named-command eshell-named-command-hook))))

(provide 'msystem)

;; NOTE:
;; * useful references
;; eshell-parse-command
;; https://github.com/emacs-china/hello-emacs/blob/master/Emacs中的shell--Eshell使用笔记.org

;;; msystem.el ends here
