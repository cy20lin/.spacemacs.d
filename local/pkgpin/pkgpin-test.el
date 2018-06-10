;; https://stackoverflow.com/questions/13866848/how-to-save-a-list-of-all-the-installed-packages-in-emacs-24/39855696#39855696

(defun list-packages-and-versions ()
  "Returns a list of all installed packages and their versions"
  (mapcar
   (lambda (pkg)
     `(,pkg ,(package-desc-version
              (cadr (assq pkg package-alist)))))
   package-activated-list))

;; (pkgpin--recipe-to-git-uri (quelpa-get-melpa-recipe "company"))

(pkgpin--pin-package 'company)

(pkgpin--pin-git-recipe (quelpa-get-melpa-recipe "company"))
(pkgpin--pin-recipe (quelpa-get-melpa-recipe "company"))

(f-files (first quelpa-melpa-recipe-stores))

(pkgpin--update-package 'company)
(pkgpin--update-recipe (quelpa-get-melpa-recipe 'company))

(pkgpin--recipe-to-string (quelpa-get-melpa-recipe "company"))

(pkgpin--git-last-commit "https://github.com/nlohmann/json" "develop")
(pkgpin--git-last-commit "https://github.com/nlohmann/json" "master")
(pkgpin--git-last-commit "https://github.com/nlohmann/json")


(plist-get (cdr (quelpa-get-melpa-recipe "company")) :fetcher)
;; repo only -> (url head)
;; repo with commit
;; repo with branch ->
;; update rule
;; no commit, no branch -> default branch, :commit <id>
;; with commit, no branch

;; (defun pkgpin--git-latest-commit-2 (repo-uri &optional cache-dir git-command)
;;   (second (s-match "^\\([a-z0-9]\\{40\\}\\) +HE AD" (first (process-lines "git" "ls-remote" repo-uri "HEAD"))))
;;   )
;; "^\\([a-z0-9]\\{40\\}\\)\tHEAD"


(pkgpin--git-latest-commit "https://github.com/cy20lin/archer")
(car (process-lines "git" "ls-remote" "https://github.com/cy20lin/archer" "HEAD"))
recipe-to-git-repo-url
;; (second (s-match "^\\([a-z0-9]\\{40\\}\\) +HEAD"
;;                  "e830bc502fe38852654a1f03f963001fecb54a86  HEAD"
;;                  ))
(s-split " " (car (pkgpin--git-latest-commit "http://github.com/nlohmann/json")))



(defvar pkgpin-cache-dir (concat (file-name-as-directory user-emacs-directory ".cache/pkgpin/")))
(defun pkgpin--)
(jk)
(quelpa-file-version )

(f-mkdir (url-hexify-string "/tmpasasdf"))
(f-mak)
(defun pkgpin--get-clone-)
(quelpa-get-melpa-recipe)

;; (pkgpin-snapshot-activated-packages)

(member 'anaconde-mode package-activated-list)
(my-configuration-layer//install-from-private-recipe-or-elpa 'anaconda-mode)
(pkgpin--pin-package 'anaconda-mode)
;; (pkgpin--update-package 'helm)
;; (pkgpin--pin-recipe (quelpa-get-melpa-recipe 'flycheck))

;; (pkgpin--recipe-to-git-uri (quelpa-get-melpa-recipe 'flycheck))
;; (pkgpin--pin-git-recipe (quelpa-get-melpa-recipe 'flycheck))
(quelpa (append (cons 'bbb (cdr (my-configuration-layer//get-private-recipe 'let-alist))) '(:files ("let-alist-1.0.5.el"))))
