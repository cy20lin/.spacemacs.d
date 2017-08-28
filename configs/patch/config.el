
;; Apply this patch to enable `SPC f o' to open file in external app in neotree
(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs//open-in-external-app (expand-file-name default-directory))
    ;;- (let ((file-path (if (derived-mode-p 'dired-mode)
    ;;-                      (dired-get-file-for-visit)
    ;;-                    buffer-file-name)))
    ;;- (eq major-mode 'neotree-mode)
    (let ((file-path (cond ((derived-mode-p 'dired-mode) (dired-get-file-for-visit))
                           ((derived-mode-p 'neotree-mode) (or (neo-buffer--get-filename-current-line) neo-buffer--start-node))
                           (t (buffer-file-name)))))
      (if file-path
          (spacemacs//open-in-external-app file-path)
        (message "No file associated to this buffer.")))))


;; Workaround for MSYS2 MSYS-Shell
(defun server-ensure-safe-dir (dir)
  "Make sure DIR is a directory with no race-condition issues.
Creates the directory if necessary and makes sure:
- there's no symlink involved
- it's owned by us
- it's not readable/writable by anybody else."
  (setq dir (directory-file-name dir))
  (let ((attrs (file-attributes dir 'integer)))
    (unless attrs
      (cl-letf (((default-file-modes) ?\700)) (make-directory dir t))
      (setq attrs (file-attributes dir 'integer)))

    ;; Check that it's safe for use.
    (let* ((uid (nth 2 attrs))
	   (w32 (eq system-type 'windows-nt))
	   (safe (cond
		  ((not (eq t (car attrs))) nil)  ; is a dir?
		  ((and w32 (zerop uid))	  ; on FAT32?
		   (display-warning
		    'server
		    (format-message "\
Using `%s' to store Emacs-server authentication files.
Directories on FAT32 filesystems are NOT secure against tampering.
See variable `server-auth-dir' for details."
			    (file-name-as-directory dir))
		    :warning)
		   t)
		  ((and (/= uid (user-uid))	  ; is the dir ours?
			(or (not w32)
			    ;; Files created on Windows by Administrator
			    ;; (RID=500) have the Administrators (RID=544)
			    ;; group recorded as the owner.
			    (/= uid 544) (/= (user-uid) 500)))
		   nil)
		  (w32 t)			  ; on NTFS?
      ((and (eq system-type 'cygwin) (string-equal (getenv "MSYSTEM") "MSYS")) t) ; patch for MSYS system
		  (t				  ; else, check permissions
		   (zerop (logand ?\077 (file-modes dir)))))))
      (unless safe
        (error "The directory `%s' is unsafe" dir)))))
