(defun my-blog//blog-admin-mode-init ()
  "Setup blog-admin buffer local variables."
  (when (configuration-layer/package-usedp 'projectile)
    (let ((path (ignore-errors (projectile-project-root))))
      (when path
        (message "path %s" path)
        (setq-local blog-admin-backend-path path))))
  (define-key blog-admin-mode-map (kbd "SPC") nil)
  ;; (lookup-key blog-admin-mode-map (kbd "SPC"))
  )
