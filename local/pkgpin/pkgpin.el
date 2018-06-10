(defconst pkgpin-version "0.1.0")

(defvar pkgpin-recipe-dir "~/.spacemacs.d/recipes/snapshot/")

(defun pkgpin--git-last-head-commit (repo-uri)
  (or (let* ((fields (s-split "\t" (first (process-lines "git" "ls-remote" repo-uri "HEAD"))))
             (sha (first fields))
             (ref (second fields))
             (valid (and (eq 40 (length sha)) (string-equal "HEAD" ref)))
             (result (if valid sha)))
        result)
      ;; --symref
      (pkgpin--git-last-branch-commit repo-uri "master")))

(defun pkgpin--git-last-branch-commit (repo-uri branch)
  (let* ((fields (s-split "\t" (first (process-lines "git" "ls-remote" "--heads" repo-uri (concat "refs/heads/" branch)))))
         (sha (first fields))
         (ref (second fields))
         (valid (and (eq 40 (length sha)) (string-equal (concat "refs/heads/" branch) ref)))
         (result (if valid sha)))
    result))

(defun pkgpin--git-last-commit (repo-uri &optional branch)
  (if branch
      (pkgpin--git-last-branch-commit repo-uri branch)
    (pkgpin--git-last-head-commit repo-uri)))

(defun pkgpin--recipe-to-git-uri (recipe)
  (let* ((package (car recipe))
         (plist- (cdr recipe))
         (fetcher (plist-get plist- :fetcher))
         (url-or-repo (plist-get plist- (if (eq fetcher 'git) :url :repo)))
         (url-or-repo- (if (symbolp url-or-repo) (symbol-name url-or-repo) url-or-repo))
         (uri (cond ((eq fetcher 'git) url-or-repo)
                    ((eq fetcher 'github) (concat "https://github.com/" url-or-repo-))
                    ((eq fetcher 'gitlab) (concat "https://gitlab.com/" url-or-repo-)))))
    uri))

(defun pkgpin--pin-git-recipe (recipe)
  (let* ((package (car recipe))
         (plist- (cdr recipe))
         (branch (plist-get plist- :branch))
         (commit- (plist-get plist- :commit))
         (uri (pkgpin--recipe-to-git-uri recipe))
         (commit (if commit- commit- (pkgpin--git-last-commit uri branch)))
         (new-recipe (if commit- recipe (if commit (append recipe (list :commit commit))))))
    new-recipe))

(defun pkgpin--pin-recipe (recipe)
  ;; return nil if cannot pin
  (if recipe
      (let* ((package (car recipe))
             (plist- (cdr recipe))
             (fetcher (plist-get plist- :fetcher)))
        (if (or (eq fetcher 'git) (eq fetcher 'github) (eq fetcher 'gitlab))
            (pkgpin--pin-git-recipe recipe)))))

(defun pkgpin--pin-package (package)
  (pkgpin--pin-recipe (quelpa-get-melpa-recipe package)))

(defun pkgpin--save-recipe (recipe &optional dir)
  (if recipe
      (let* (
             (package (car recipe))
             (package-name (symbol-name package))
             (plist- (cdr recipe))
             (recipe-file-path (f-join pkgpin-recipe-dir package-name))
             (recipe-string (pkgpin--recipe-to-string recipe))
             )
        ;; TODO: Make sure directory exist before write
        ;; TODO: writing policies => overwrite, ignore ...
        (f-write-bytes (format "%s\n" recipe-string) recipe-file-path)
        (message "[pkgpin] pin package: %s => %s" package-name recipe-string))))

(defun pkgpin--update-package (package &optional dir)
  (pkgpin--save-recipe (pkgpin--pin-package package)))

(defun pkgpin--recipe-to-string (recipe)
  ""
  (format "%S" recipe))

(defun pkgpin-snapshot-activated-packages ()
  ;; (&optional snapshot-dir file-name-format)
  (interactive)
  (dolist (package package-activated-list)
    (pkgpin--update-package package)))

