(defun my-c-c++-cdb-guess (command &rest args)
  ""
  (cl-case command
    (get-compile-options (my-c-c++-cdb-guess--get-compile-options))))

(defun my-c-c++-cdb-guess--split-environment-path (&optional paths-string separator)
  ""
  (let ((paths-string (or paths-string (getenv "PATH"))))
    (cond
     ((eq system-type 'windows-nt)
      (mapcar #'(lambda (s) (subst-char-in-string ?\\ ?/ s))
              (split-string-and-unquote paths-string (or separator ";"))))
     (t (split-string-and-unquote (getenv "PATH") (or separator ":"))))))

(defun my-c-c++-cdb-guess--add-prefix-foreach (prefix sequence)
  (mapcar (lambda (str) (concat prefix str)) sequence))

(defun my-c-c++-cdb-guess--filter-bin-to-include (paths)
  (mapcar
   (lambda (x) (f-join (f-dirname x) "include"))
   (remove-if-not (lambda (x) (equal (f-filename x) "bin")) paths)))

(defun my-c-c++-cdb-guess-system-include-from-exec-path ()
  (my-c-c++-cdb-guess--add-prefix-foreach
   "-isystem"
   (my-c-c++-cdb-guess--filter-bin-to-include
    (my-c-c++-cdb-guess--split-environment-path))))

(defun my-c-c++-cdb-guess-user-include-from-projectile ()
  ""
  (if (projectile-project-p)
      (let ((source-dir (projectile-project-root))
            (build-dir (projectile-compilation-dir)))
        (my-c-c++-cdb-guess--add-prefix-foreach
         "-I"
         (if (string-equal source-dir build-dir)
             (list source-dir (f-join source-dir "include") (f-join source-dir "src"))
           (list build-dir source-dir (f-join source-dir "include") (f-join source-dir "src")))))))

(defcustom my-c-c++-cdb-guess-guess-handlers
 (list
  #'my-c-c++-cdb-guess-system-include-from-exec-path
  #'my-c-c++-cdb-guess-user-include-from-projectile
  #'my-c-c++-cdb-guess-language-standard-flag
  )
 "List of guess handlers"
 )

(defun my-c-c++-cdb-guess--get-compile-options ()
  ""
  (let ((flags (mapcan #'identity (mapcar #'funcall my-c-c++-cdb-guess-guess-handlers))))
    (if flags
        ;; (if (projectile-project-p)
        ;;     (cons (list (append flags (list (buffer-file-name)))) (projectile-compilation-dir))
        ;;   (cons (list (append flags (list (buffer-file-name)))) default-directory))
        (if (projectile-project-p)
            (list (cons flags (projectile-compilation-dir)))
          (list (cons flags default-directory)))
      )))

(provide 'my-c-c++-cdb-guess)
