;; https://stackoverflow.com/questions/14737104/what-is-the-default-c-mode-for-the-current-gcc-especially-on-ubuntu
;; -std=c11 -pedantic

(defun my-c-c++-cdb-executable-find (command)
  (executable-find command))

(defun my-c-c++-cdb-executable-version (command)
  (let* ((exe (my-c-c++-cdb-executable-find command))
         (cmd (combine-and-quote-strings (list exe "--version"))))
    (if exe (car (s-match "[0-9]\\.[0-9]\\.[0-9]" (shell-command-to-string (combine-and-quote-strings (list exe "--version"))))))))

(defvar my-c-c++-cdb-clang++-version-to-standard
  '((">=5.0.0" . "c++17")
    (">=3.4.0" . "c++14")
    (">=3.3.0" . "c++11")
    (t . nil)))

(defvar my-c-c++-cdb-clang-version-to-standard
  '((">=3.1" . "c11")
    (t . nil)))

(defvar my-c-c++-cdb-g++-version-to-standard
  '((">=7.2.0" . "c++17")
    (">=6.1.0" . "c++14")
    (">=4.8.0" . "c++11")
    (t . nil)))

(defvar my-c-c++-cdb-gcc-version-to-standard
  '((">=4.7.0" . "c11")
    (t . nil)))

(defun my-c-c++-cdb-find-clang++-language-standard (&optional command)
  (let ((version (my-c-c++-cdb-executable-version (or nil "clang++")))
        (pred nil)
        (standard nil)
        )
    (dolist (pair my-c-c++-cdb-clang++-version-to-standard)
      (setq pred (car pair))
      (setq standard (cdr pair))
      (if (my-c-c++-cdb-version-verify pred version) (return)))
    standard))

(defun my-c-c++-cdb-find-clang-language-standard (&optional command)
  (let ((version (my-c-c++-cdb-executable-version (or nil "clang")))
        (pred nil)
        (standard nil)
        )
    (dolist (pair my-c-c++-cdb-clang-version-to-standard)
      (setq pred (car pair))
      (setq standard (cdr pair))
      (if (my-c-c++-cdb-version-verify pred version) (return)))
    standard))

(defun my-c-c++-cdb-find-gcc-language-standard (&optional command)
  (let ((version (my-c-c++-cdb-executable-version (or nil "gcc")))
        (pred nil)
        (standard nil)
        )
    (dolist (pair my-c-c++-cdb-gcc-version-to-standard)
      (setq pred (car pair))
      (setq standard (cdr pair))
      (if (my-c-c++-cdb-version-verify pred version) (return)))
    standard))

(defun my-c-c++-cdb-find-g++-language-standard (&optional command)
  (let ((version (my-c-c++-cdb-executable-version (or nil "g++")))
        (pred nil)
        (standard nil)
        )
    (dolist (pair my-c-c++-cdb-g++-version-to-standard)
      (setq pred (car pair))
      (setq standard (cdr pair))
      (if (my-c-c++-cdb-version-verify pred version) (return)))
    standard))

(defun my-c-c++-cdb-version-compare (op ver1 ver2)
  (let ((op (if (stringp op) (intern op) op))
        (ver1 (version-to-list ver1))
        (ver2 (version-to-list ver2)))
    (pcase op
      ('< (version-list-< ver1 ver2))
      ('> (version-list-< ver2 ver1))
      ('>= (version-list-<= ver1 ver2))
      ('<= (version-list-<= ver2 ver1))
      ('!= (not (version-list-= ver1 ver2)))
      ('== (version-list-= ver1 ver2))
      ('= (version-list-= ver1 ver2))
      (_ nil))))

;; (my-c-c++-cdb-version-compare "<" "1.2.3" "2.3.4")
;; (my-c-c++-cdb-version-compare '< "1.2.3" "2.3.4")
;; (my-c-c++-cdb-version-compare ">" "1.2.3" "2.3.4")

(defun my-c-c++-cdb-version-verify (version-pred version)
  (cond ((stringp version-pred) (let* ((matches (s-match "^\\([^a-zA-Z0-9]*\\)\\([0-9]+\\(?:\\.[0-9]+\\)\\{,3\\}\\)$" version-pred))
                                       (op (second matches))
                                       (version-str (third matches)))
                                  (my-c-c++-cdb-version-compare op version-str version)))
        ((null nil) nil)
        (t t)))

;; (my-c-c++-cdb-version-verify ">=1.2.3" "1.2.3")

(defun my-c-c++-cdb-split-version (version-expr)
  (let ((version (car (s-match "[0-9]\\.[0-9]\\.[0-9]" version-expr))))
    (if version (mapcar (function string-to-number) (s-split "\\." version)))))

(defun my-c-c++-cdb-guess-clang-standard-flag (&optional mode)
  (let ((mode (or mode major-mode)))
    (cond ((eq mode 'c++-mode) (concat "-std=" (my-c-c++-cdb-find-clang++-language-standard)))
          ((eq mode 'c-mode) (concat "-std=" (my-c-c++-cdb-find-clang-language-standard)))
          (t nil))))

(defun my-c-c++-cdb-guess-gcc-standard-flag (&optional mode)
  (let ((mode (or mode major-mode)))
    (cond ((eq mode 'c++-mode) (concat "-std=" (my-c-c++-cdb-find-clang++-language-standard)))
          ((eq mode 'c-mode) (concat "-std=" (my-c-c++-cdb-find-clang-language-standard)))
          (t nil))))

(defun my-c-c++-cdb-guess-clang-standard (&optional mode)
  (let ((mode (or mode major-mode)))
    (cond ((eq mode 'c++-mode) (my-c-c++-cdb-find-clang++-language-standard))
          ((eq mode 'c-mode) (my-c-c++-cdb-find-clang-language-standard))
          (t nil))))

(defun my-c-c++-cdb-guess-gcc-standard (&optional mode)
  (let ((mode (or mode major-mode)))
    (cond ((eq mode 'c++-mode) (my-c-c++-cdb-find-clang++-language-standard))
          ((eq mode 'c-mode) (my-c-c++-cdb-find-clang-language-standard))
          (t nil))))

(defun my-c-c++-cdb-guess-language-standard-flag (&optional mode)
  (or (list (my-c-c++-cdb-guess-clang-standard-flag))
      (list (my-c-c++-cdb-guess-gcc-standard-flag))
      nil))

(provide 'my-c-c++-cdb-guess-standard)
