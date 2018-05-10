
(defun my-c-c++-cdb-guess-compiler-include-dirs (&optional compiler)
  (let ((compiler- (executable-find (or compiler "gcc"))))
    (let* ((exe-dir (directory-file-name (file-name-directory compiler-)))
           (exe-dir-name (file-name-nondirectory exe-dir))
           (prefix (if (equal exe-dir-name "bin") (directory-file-name (file-name-directory exe-dir))))
           (local-prefix (concat prefix "/local")))
      (if prefix (list (concat prefix "/include") (concat local-prefix "/include"))))))

(defun my-c-c++-cdb-guess-compiler-include-flags (&optional compiler)
  (let* ((dirs (my-c-c++-cdb-guess-compiler-include-dirs))
        (include-dir (first dirs))
        (local-include-dir (second dirs)))
    (if include-dir (list "-isystem" include-dir "-isystem" local-include-dir))))

;; (my-c-c++-cdb-guess-compiler-include-dirs)
;; (my-c-c++-cdb-guess-compiler-include-flags)

(provide 'my-c-c++-cdb-guess-compiler-includes)
