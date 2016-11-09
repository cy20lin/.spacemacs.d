;;http://emacs.stackexchange.com/questions/19536/why-do-paths-start-with-c-c-in-windows-emacs-when-i-use-next-error
;; (defun msys-drive-name-handler (operation &rest args)
;;   "Call `unmsys--file-name' on file names."
;;   (let ((inhibit-file-name-handlers
;;          (cons 'msys-file-name-handler
;;                (and (eq inhibit-file-name-operation operation)
;;                     inhibit-file-name-handlers)))
;;         (inhibit-file-name-operation operation))
;;     (pcase (cons operation args)
;;       (`(expand-file-name ,name . ,(or `(,directory) directory))
;;        (expand-file-name (unmsys--file-name name) (if directory (unmsys--file-name directory))))
;;       (`(substitute-in-file-name ,name)
;;        (substitute-in-file-name (unmsys--file-name name)))
;;       (_ (apply operation args)))))

;; [note] this handler will not affect `f-expand',
;; but it will affect `f-exists?'
;; using (directory-file-name (expand-file-name path dir)) instead of `f-expand'
;; (push '("\\`/[a-zA-Z]/" . msys-drive-name-handler) file-name-handler-alist)

(require 's)
(defun my-msystem-expand-file-name (path)
  "convert path in msys style to (foward slashed) windows style"
  (cond
   ;; /d/path/to/file => d:/path/to/file
   ((s-match "^/[a-zA-Z]/" path) (aset path 0 (aref path 1)) (aset path 1 ?:) path)
   ;; /path/to/file => drive:/msystem/prefix/directory/path/to/file
   ((s-starts-with? "/" path) (concat my-msystem-prefix (s-chop-prefix "/" path)))
   ;; ~/path/to/file => drive:/msystem/prefix/home/username/path/to/file
   ((s-starts-with? "~" path)  my-msystem-home))
  ((s-starts-with? "~/" path) (concat my-msystem-home (s-chop-prefix "~/" path)))
  ;; ~username/path/to/file =>
  ;; (currently not support)
  (t (expand-file-name path)))
