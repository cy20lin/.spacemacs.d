(require 'json)
(require 's)
(require 'f)

;; cppcheck-c/c++-gcc-executable
;; cppcheck-c/c++-clang-executable
;; cppcheck-c/c++-cppcheck-executable
(defcustom my-flycheck-cxx-args nil
  "A list of additional arguments for c/c++-clang and c/c++-gcc respectively. ")
(defcustom my-flycheck-cxx-blocks nil
  "Whether to enable blocks in c/c++-clang. ")
(defcustom my-flycheck-cxx-definitions nil
  "A list of additional preprocessor definitions for c/c++-clang and c/c++-gcc respectively.")
(defcustom my-flycheck-cxx-include-path nil
  "A list of include directories for c/c++-clang and c/c++-gcc respectively, relative to the file being checked.")
(defcustom my-flycheck-cxx-includes nil
  "A list of additional include files for c/c++-clang and c/c++-gcc respectively, relative to the file being checked.")
(defcustom my-flycheck-cxx-language-standard nil
  "The language standard to use in c/c++-clang and c/c++-gcc respectively as string, via the -std option.")
(defcustom my-flycheck-cxx-ms-extensions nil
  "Whether to enable Microsoft extensions to C/C++ in c/c++-clang. ")
(defcustom my-flycheck-cxx-no-exceptions nil
  "Whether to disable exceptions in c/c++-clang and c/c++-gcc respectively. ")
(defcustom my-flycheck-cxx-no-rtti nil
  "Whether to disable RTTI in c/c++-clang and c/c++-gcc respectively, via -fno-rtti.")
(defcustom my-flycheck-cxx-standard-library nil
  "The name of the standard library to use for c/c++-clang, as string.")
(defcustom my-flycheck-cxx-openmp nil
  "Whether to enable OpenMP in c/c++-gcc.")
(defcustom my-flycheck-cxx-pedantic nil
  "Whether to warn about language extensions in c/c++-clang and c/c++-gcc respectively.")
(defcustom my-flycheck-cxx-pedantic-errors nil
  "Whether to error on language extensions in c/c++-clang and c/c++-gcc respectively.")
(defcustom my-flycheck-cxx-warnings nil
  "A list of additional warnings to enable in c/c++-clang and c/c++-gcc respectively. Each item is the name of a warning or warning category for -W.")
;; defcustom my-flycheck-cppcheck-checks
;; defcustom my-flycheck-cppcheck-inconclusive
;; defcustom my-flycheck-cppcheck-include-path
;; defcustom my-flycheck-cppcheck-standards
;; defcustom my-flycheck-cppcheck-suppressions

(defun my-flycheck-get-checker-executable (checker)
  (flycheck-checker-get checker 'file))

(defun my-cxx--setup-flycheck-cxx-checker-variables (checker)
  ""
  (cond
   ((eq checker 'c/c++-gcc)
    (setq flycheck-gcc-args my-flycheck-cxx-args)
    ;; (setq flycheck-gcc-blocks my-flycheck-cxx-blocks)
    (setq flycheck-gcc-definitions my-flycheck-cxx-definitions)
    (setq flycheck-gcc-include-path my-flycheck-cxx-include-path)
    (setq flycheck-gcc-includes my-flycheck-cxx-includes)
    (setq flycheck-gcc-language-standard my-flycheck-cxx-language-standard)
    ;; (setq flycheck-gcc-ms-extensions my-flycheck-cxx-ms-extensions)
    (setq flycheck-gcc-no-exceptions my-flycheck-cxx-no-exceptions)
    (setq flycheck-gcc-no-rtti my-flycheck-cxx-no-rtti)
    ;; (setq flycheck-gcc-standard-library my-flycheck-cxx-standard-library)
    (setq flycheck-gcc-openmp my-flycheck-cxx-openmp)
    (setq flycheck-gcc-pedantic my-flycheck-cxx-pedantic)
    (setq flycheck-gcc-pedantic-errors my-flycheck-cxx-pedantic-errors)
    (setq flycheck-gcc-warnings my-flycheck-cxx-warnings)
    t)
   ((eq checker 'c/c++-clang)
    (setq flycheck-clang-args my-flycheck-cxx-args)
    (setq flycheck-clang-blocks my-flycheck-cxx-blocks)
    (setq flycheck-clang-definitions my-flycheck-cxx-definitions)
    (setq flycheck-clang-include-path my-flycheck-cxx-include-path)
    (setq flycheck-clang-includes my-flycheck-cxx-includes)
    (setq flycheck-clang-language-standard my-flycheck-cxx-language-standard)
    (setq flycheck-clang-ms-extensions my-flycheck-cxx-ms-extensions)
    (setq flycheck-clang-no-exceptions my-flycheck-cxx-no-exceptions)
    (setq flycheck-clang-no-rtti my-flycheck-cxx-no-rtti)
    (setq flycheck-clang-standard-library my-flycheck-cxx-standard-library)
    ;; (setq flycheck-clang-openmp my-flycheck-cxx-openmp)
    (setq flycheck-clang-pedantic my-flycheck-cxx-pedantic)
    (setq flycheck-clang-pedantic-errors my-flycheck-cxx-pedantic-errors)
    (setq flycheck-clang-warnings my-flycheck-cxx-warnings)
    t)
   ;; ((eq checker 'c\c++-cppcheck) nil)
   ;; ((eq checker 'c\c++-irony) nil)
   ;; ((eq checker 'c\c++-ycmd) nil)
   (t nil)))

(defun my-cxx--find-nearest (base-dir filename)
  ""
  (do ((dir base-dir (setq dir (f-dirname dir))))
      ((equal dir nil))
    (let ((file-fullpath (f-join dir filename)))
      (if (f-exists? file-fullpath)
          (return file-fullpath)))))


(defun my-cxx--setup-flycheck-cxx-checkers-variables (checkers)
  ""
  (let ((count 0))
    (dolist (checker checkers)
      (if (my-setup-flycheck-cxx-checker-variables checker)
          (incf count)))
    count))

(defun my-cxx--strip-unneeded-flags-for-flycheck (flags)
  ""
  (let ((new-flags nil)
        (delete-next-flag? nil)
        (unneeded-flags '("-o" "-c" "-S" "-E"))
        (new-flag nil))
    (loop for flag in flags do
          (setq new-flag flag)
          (if delete-next-flag?
              (progn ;; then
                (setq new-flag nil)
                (setq delete-next-flag? nil))
            (progn ;; else
              (loop for unneeded-flag in unneeded-flags do
                    (cond
                     ((equal unneeded-flag flag)
                      (setq delete-next-flag? t)
                      (setq new-flag nil)
                      (return))
                     ((s-starts-with? unneeded-flag flag)
                      (setq new-flag nil)
                      (return)))))
            (if new-flag (setq new-flags (cons new-flag new-flags)))))
    (reverse new-flags)))

(defun my-cxx--make-realtive-paths-in-flags-absolute (flags working-dir)
  ""
  (let ((new-flags nil)
        (make-next-absolute nil)
        (path-flags '("-isystem" "-I" "-iqoute" "--sysroot="))
        (new-flag nil))
    (loop for flag in flags do
          (setq new-flag flag)
          (if make-next-absolute
              (progn ;; then
                (setq make-next-absolute nil)
                (if (not (s-starts-with? "/" flag))
                    (setq  new-flag (f-join working-dir flag))))
            (progn ;; else
              (loop for path-flag in path-flags do
                    (cond
                     ((equal flag path-flag)
                      (setq make-next-absolute t)
                      (return nil))
                     ((s-starts-with? path-flag flag)
                      (setq new-flag
                            (concat
                             path-flag
                             (f-join working-dir (s-chop-prefix path-flag flag))))
                      (return nil))))))
          (if new-flag (setq new-flags (cons new-flag new-flags))))
    (reverse new-flags)
    ))

(defun my-cxx--project-include-dirs (search-root)
  ""
  (let* ((include (my-cxx--find-nearest search-root "include"))
         (src (my-cxx--find-nearest search-root "src"))
         (external-include (my-cxx--find-nearest search-root "external/include"))
         (project-include (cond (src (f-dirname src))
                                (include (f-dirname include))))
         ;; external-include should be considered as a system include directory
         ;; another way to find external-include
         ;; (external-include (and project-include (f-join project-include "external/include")))
         )
    (remove nil (list external-include include project-include))
    )
  )

(defun my-cxx--project-include-flags (search-root)
  ""
  ;; [TODO]
  ;; define buffer local variable for guessed paths
  ;; defun find-nearest files
  (let* ((include (my-cxx--find-nearest search-root "include"))
         (src (my-cxx--find-nearest search-root "src"))
         (test (my-cxx--find-nearest search-root "test"))
         (project-include (cond (src (f-dirname src))
                                (include (f-dirname include))
                                (test (f-dirname test))))
         (external-include (my-cxx--find-nearest search-root "external/include"))
         ;; (external-include (or (my-cxx--find-nearest search-root "external/include")
         ;;                        (and project-include (f-join project-include "external/include"))))
         ;; external-include should be considered as a system include directory
         ;; another way to find external-include
         ;; (external-include (and project-include (f-join project-include "external/include")))
         )
    (remove nil (list
                 (and include (concat "-I" include))
                 (and project-include (concat "-I" project-include))
                 (and external-include (concat "-isystem" external-include))))))

(defun my-cxx--system-include-dirs (compiler-path)
  ""
  (let* ((include (f-join (f-dirname compiler-path) "../include"))
         (local-include (f-join include "../local/include")))
    (list local-include include)))

(defun my-cxx--system-include-flags (compiler-path)
  ""
  (mapcar #'(lambda (dir) (concat "-isystem" dir))
          (my-cxx--system-include-dirs compiler-path)
          ))

(defun my-cxx--find-compilation-database (search-root)
  ""
  (let ((db-path nil))
    (loop for db-name in `("build/compile_commands.json" "compile_commands.json") do
          (setq db-path (my-cxx--find-nearest search-root db-name))
          (if db-path (return db-path)))))

(defun my-cxx--get-flags-in-compliation-database-by-filename (filename)
  (let* ((root (f-dirname filename))
         (db-path (my-cxx--find-compilation-database root)))
    (if db-path
        (let* ((db (json-read-file db-path))
               (info (my-cxx--get-compliation-info-in-database-by-filename db filename))
               (working-dir   (cdr (assq 'directory info)))
               (command-str   (cdr (assq 'command info)))
               (command       (and command-str (s-split " " command-str t)))
               (compiler-path (car command))
               (old-flags     (cdr command))
               (new-flags (my-cxx--make-realtive-paths-in-flags-absolute old-flags working-dir)))
          (my-cxx--strip-unneeded-flags-for-flycheck new-flags)
          ))))

(defvar my-cxx--c-default-flags
  (list "-std=c11")
  "c default flags"
  )

(defvar my-cxx--c++-default-flags
  (list "-std=c++14")
  "c++ default flags"
  )

(defun my-cxx--guess-flags (filename &optional major-mode- compiler db-path)
  (let ((db-flags (my-cxx--get-flags-in-compliation-database-by-filename filename)))
    ;; if db-flags is nil, it means that
    ;; 1. compile_commands.json not found
    ;; 2. compile_commands.json found, but the file doesn't exist in the database
    ;; 3. there is no flags for that file in compile_commands.json
    ;; in these situations, using fallback flags
    (or db-flags
        (let* ((compiler-path (or
                               compiler
                               (funcall flycheck-executable-find "gcc")
                               (funcall flycheck-executable-find "clang")))
               (system-include-flags (and compiler-path
                                          (my-cxx--system-include-flags compiler-path)))
               (project-include-flags (my-cxx--project-include-flags filename))
               (default-flags (my-cxx--guess-default-flags filename major-mode-))
               )
          (append default-flags project-include-flags system-include-flags)))))

(defvar my-cxx-c-source-extensions
  (list
   ".c"
   ))
(defvar my-cxx-c-header-extensions
  (list
   ".h"
   ))
(defvar my-cxx-c++-source-extensions
  (list
   ".cpp"
   ".cc"
   ".cxx"
   ))

(defvar my-cxx-c++-header-extensions
  (list
   ".hpp"
   ".ipp"
   ".hh"
   ".hxx"
   ".ixx"
   ".def"
   ))

(defun my-cxx--guess-default-flags (filename &optional major-mode-)
  (my-cxx--get-default-flags-by-file-type
   (my-cxx--guess-file-type filename major-mode-))
  )

(defun my-cxx--guess-file-type (filename &optional major-mode-)
  "guess file type by filename"
  ;; currently, using rough guess to get its file type
  ;; it doesnt check extensions strickly
  ;; maybe I should use `f-ext?' and instead ?
  ;; that is extreme case
  (let ((file-type
         (and filename
              (block this
                (loop for ext in my-cxx-c-source-extensions do
                      (if (s-ends-with? ext filename) (return-from this '(c  source))))
                (loop for ext in my-cxx-c++-source-extensions do
                      (if (s-ends-with? ext filename) (return-from this '(c++  source))))
                (loop for ext in my-cxx-c-header-extensions do
                      (if (s-ends-with? ext filename) (return-from this '(c  header))))
                (loop for ext in my-cxx-c++-header-extensions do
                      (if (s-ends-with? ext filename) (return-from this '(c++  header))))
                (if (s-ends-with? ".cmake" filename) (return-from this '(cmake)))
                ;; (if (s-ends-with? "/CMakeLists.txt" filename) (return-from this '(cmake)))
                (if (s-ends-with? "CMakeLists.txt" filename) (return-from this '(cmake)))
                ))))
    (cond
     ((eq major-mode- 'c-mode) (cons 'c (second file-type)))
     ((eq major-mode- 'c++-mode) (cons 'c++ (second file-type)))
     ((eq major-mode- 'cmake-mode) (cons 'cmake nil))
     (t file-type))))

(defun my-cxx--get-default-flags-by-file-type (file-type)
  ""
  (case (car file-type)
    ('c++ my-cxx--c++-default-flags)
    ('c   my-cxx--c-default-flags)
    (otherwise nil)))

(defun my-cxx--get-compliation-info-in-database-by-filename (db filename )
  ""
  (dotimes (i (length db))
    (let* ((info (aref db i))
           (filename- (cdr (assq 'file info)))
           (found? (f-same? filename filename-)))
      (if found? (return info)))))

;; (defun my-cxx-flychek-setup-current-buffer-flycheck-environment ()
;;   ""
;;   (let ((filename (buffer-file-name))
;;         (flags (my-cxx--get-flags-by-filename filename))
;;         )
;;     (set (make-local-variable 'flycheck-gcc-args) flags)
;;     (set (make-local-variable 'flycheck-clang-args) flags)))

(defun my-cxx--c-c++-compile-command ()
  (format "cd \"%s\" && cmake --build build --target all --config Release" (projectile-project-root))
  )

(defun my-cxx--cmake-compile-command ()
  (format "cd \"%s\" && ( test ! -e ./build || rm -rf ./build ) && mkdir build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G\"Ninja\" -H. -Bbuild && ~/.spacemacs.d/tools/normalize-path.sh ./build/compile_commands.json"
          (projectile-project-root) my-user-spacemacs-directory))

(defun my-cxx-setup-local-commands ()
  (case major-mode
    ('c++-mode
     (set (make-local-variable 'compile-command) (my-cxx--c-c++-compile-command)))
    ('c-mode
     (set (make-local-variable 'compile-command) (my-cxx--c-c++-compile-command)))
    ('cmake-mode
     (set (make-local-variable 'compile-command) (my-cxx--cmake-compile-command)))
    (otherwise nil)))

(defun my-cxx-setup-local-projectile-variables ()
  ""
  ;; projectile commands
  ;; for compilation - projectile-project-compilation-cmd
  ;; for testing - projectile-project-test-cmd
  ;; for running - projectile-project-run-cmd
  (case major-mode
    ('c++-mode
     (set 'projectile-project-compilation-cmd (my-cxx--c-c++-compile-command))
     ;; (set (make-local-variable 'projectile-project-compilation-cmd) (my-cxx--c-c++-compile-command))
     ;; (set (make-local-variable 'projectile-project-test-cmd) (my-cxx--c-c++-test-command))
     ;; (set (make-local-variable 'projectile-project-run-cmd) (my-cxx--c-c++-run-command))
     )
    ('c-mode
     (set (make-local-variable 'projectile-project-compilation-cmd) (my-cxx--c-c++-compile-command))
     ;; (set (make-local-variable 'projectile-project-test-cmd) (my-cxx--c-c++-test-command))
     ;; (set (make-local-variable 'projectile-project-run-cmd) (my-cxx--c-c++-run-command))
     )
    ('cmake-mode
     (set (make-local-variable 'projectile-project-compilation-cmd) (my-cxx--cmake-compile-command))
     ;; (set (make-local-variable 'projectile-project-test-cmd) (my-cxx--cmake-compile-command))
     ;; (set (make-local-variable 'projectile-project-run-cmd) (my-cxx--cmake-compile-command))
     )
    (otherwise nil)))

(defun my-cxx-setup-local-flycheck-variables ()
  ""
  (let ((flags (my-cxx--guess-flags (buffer-file-name) major-mode)))
    (set (make-local-variable 'flycheck-clang-args) flags)
    (set (make-local-variable 'flycheck-gcc-args) flags)
    ))

(defun my-cxx-register-hooks ()
  (add-to-list 'c-mode-hook #'my-cxx-setup-local-flycheck-variables)
  (add-to-list 'c-mode-hook #'my-cxx-setup-local-projectile-variables)
  (add-to-list 'c-mode-hook #'my-cxx-setup-local-commands)
  (add-to-list 'c++-mode-hook #'my-cxx-setup-local-flycheck-variables)
  (add-to-list 'c++-mode-hook #'my-cxx-setup-local-projectile-variables)
  (add-to-list 'c++-mode-hook #'my-cxx-setup-local-commands)
  (add-to-list 'cmake-mode-hook #'my-cxx-setup-local-commands)
  t)

(my-cxx-register-hooks)
(with-eval-after-load "projectile" (add-to-list 'projectile-project-root-files-top-down-recurring "CMakeLists.txt"))

;; [NOTE]
;;
;; #+DATE: 20161107
;; #+AUTHOR: ChienYu Lin <cy20lin@gmail.com>
;;
;; * guess source file flags by
;; 1.1) using compiler path to guess system includes
;;      e.g.   compiler-path = path/to/bin/gcc.exe
;;          => include-dirs  = (path/to/include, path/to/local/include)
;; 1.2) using filename to guess
;;      e.g.     filename = path/to/proj/src/main.cpp
;;          find nearest include at : path/to/proj/include
;;                                    path/to/proj/external/include
;;                                    path/to/proj
;; 2) find nearest compile_commands.json or /build/compile_commands.json
;;      use the flags of that file in that database
;; * guess header file flags by
;; 1) find the same name.cpp
;; 2) using the flags that previous entered buffer used
;; 3) find source files that #include <path/to/name.hpp>
;;
;; * issue
;; 1. (loop for x in [a r r a y]) seems doenst work in emacs
;; 2. file-name-handler-alist issue in f.el
;; 2.1 (f-expand "path/to/file") and
;;     (directory-file-name (expand-file-name "path/to/file")) differs
;;     because f-expand blocks file-name-handler-alist
;; 2.2. strangely, it seems that f-exists doesn't block file-name-handler-alist
;;
;; * cheat sheet
;; ** gcc flags precedence
;; *** include flags
;; All the directories named by -I are searched, in left-to-right order, before the default directories.
;; -I flag take percedence over -isystem flag
;; e.g.
;;   gcc -isystem/first -isystem/last
;;   gcc -I/first -I/last ...
;;   gcc -I/first -isystem/last ...
;;   gcc -isystem/last -I/first ...
;;   gcc -isystem/find/here/third -I/find/here/first -isystem/find/second -isystem/find/here/fourth ...
;;
;; *** compilation standard
;; the right most flags will override previous setting
;; e.g.
;;   gcc -std=c++14 -std=c++11 ...  => using c++11 standard
;;
;; ** buffer
;; (buffer-name)
;; (buffer-file-name)
;; (current-buffer)
;; (buffer-name)
;; (window-buffer)
;; (list-buffers)
;; (window-next-buffers (current-buffer))
;; (buffer-list (current-buffer))
;; (previous-buffer)
;; (buffer-name (previous-buffer))
;; (buffer-name (first (car (window-prev-buffers))))
;; (buffer-file-name (first (car (window-prev-buffers))))
;;
;; ** alist
;; (setq alist-1 '((a . apple) (b . banana) (a . Asia) (c . cat)))
;; (setq alist-2 '(("a" . "apple") ("b" . "banana") ("a" . Asia) ("c" . "cat")))
;; (assq 'a alist-1) => (a . apple)
;; (assq "a" alist-2) => nil
;; (assoc 'a alist-1) => (a . apple)
;; (assoc "a" alist-2) => ("a" . "apple")
;; ;; `r' means reverse
;; ;; `q' means eq
;; (rassq 'apple alist-1) => (a . apple)
;; (rassq "apple" alist-2) => nil
;; (rassoc 'apple alist-1) => (a . apple)
;; (rassoc "apple" alist-2) => ("a" . "apple")
;;
;; ** array
;; (setq arr [0 1 2 3 4 5])
;; (length arr) => 6
;; (aref arr 0) => 0
;; (elt arr 0)  => 0
;; (aset arr 0 5) => 5 , where arr = [5 1 2 3 4 5]
;;
