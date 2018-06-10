;; Declare prefix for major-mode-leader too
(defun spacemacs/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'."
  (let  ((full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspacemacs-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat dotspacemacs-major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix dotspacemacs-major-mode-leader-key)
        (which-key-declare-prefixes major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix dotspacemacs-major-mode-emacs-leader-key)
        (which-key-declare-prefixes major-mode-prefix-emacs prefix-name)))))

;; Patch this someday.
;; NOTE: Maybe we can start form these funcs and vars.
;; (spacemacs-bootstrap/init-which-key)
;; spacemacs-cmds
;; (bind-key)
;; major-mode-command
;; (which-key-add-key-based-replacements)
;;
;; (defun spacemacs/set-leader-keys (key def &rest bindings)
;;   "Add KEY and DEF as key bindings under
;; `dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'.
;; KEY should be a string suitable for passing to `kbd', and it
;; should not include the leaders. DEF is most likely a quoted
;; command. See `define-key' for more information about the possible
;; choices for DEF. This function simply uses `define-key' to add
;; the bindings.

;; For convenience, this function will accept additional KEY DEF
;; pairs. For example,

;; \(spacemacs/set-leader-keys
;;    \"a\" 'command1
;;    \"C-c\" 'command2
;;    \"bb\" 'command3\)"
;;   (while key
;;     (define-key spacemacs-default-map (kbd key) def)
;;     (setq key (pop bindings) def (pop bindings))))

;;
;; #3
;;
;; (defvar my-private-melpa-recipe-stores '("~/.spacemacs.d/recipes/stable/" "~/.spacemacs.d/recipes/snapshot/"))
(defvar my-private-melpa-recipe-stores '("~/.spacemacs.d/recipes/stable/"))

(defun my-configuration-layer//get-private-recipe (name)
  (with-temp-buffer
    (setq-local quelpa-melpa-recipe-stores
                (cond
                 ((stringp my-private-melpa-recipe-stores) (list my-private-melpa-recipe-stores))
                 ((listp my-private-melpa-recipe-stores) my-private-melpa-recipe-stores)
                 (t nil)))
    (setq-local package-check-signature 'allow-unsigned)
    (quelpa-get-melpa-recipe name)))

(defun my-configuration-layer//install-from-private-recipe (pkg-name)
  ""
  (let ((recipe (my-configuration-layer//get-private-recipe pkg-name)))
    (if recipe
        (with-temp-buffer
          ;; NOTE: Maybe prepend to quelpa-melpa-recipe-stores
          (setq-local quelpa-melpa-recipe-stores
                      (append (cond
                               ((stringp my-private-melpa-recipe-stores) (list my-private-melpa-recipe-stores))
                               ((listp my-private-melpa-recipe-stores) my-private-melpa-recipe-stores)
                               (t nil))
                              quelpa-melpa-recipe-stores))
          (setq-local package-check-signature 'allow-unsigned)
          (quelpa recipe))))
  (let ((installed-p (package-installed-p pkg-name)))
    (if installed-p (message "(PatchedSpacemacs) --> private recipe package installed: %S" pkg-name))
    installed-p))

(defun my-configuration-layer//install-from-private-recipe-or-elpa (pkg-name)
  "Install PKG from QUELPA or ELPA."
  (or
   (my-configuration-layer//install-from-private-recipe pkg-name)
   (configuration-layer//install-from-elpa pkg-name)))

(defun configuration-layer//install-package (pkg)
  "Unconditionally install the package PKG."
  (message "(PatchedSpacemacs.Debug) configuration-layer//install-package: %S" pkg)
  (let* ((layer (when pkg (car (oref pkg :owners))))
         (location (when pkg (oref pkg :location)))
         (min-version (when pkg (oref pkg :min-version))))
    (message "(PatchedSpacemacs.Debug) layer=%S, location=%S, min-version=%S" layer location min-version)
    (spacemacs-buffer/replace-last-line
     (format "--> installing %s: %s%s... [%s/%s]"
             (if layer "package" "dependency")
             pkg-name (if layer (format "@%S" layer) "")
             installed-count not-inst-count) t)
    (spacemacs//redisplay)
    (unless (package-installed-p pkg-name min-version)
      (condition-case-unless-debug err
          (cond
           ((or (null pkg) (eq 'elpa location))
            (message "(PatchedSpacemacs.Debug) action=inst")
            (or
             (let ((result (progn
                             (message "(PatchedSpacemacs.Debug) action=install-from-private-recipe")
                             (my-configuration-layer//install-from-private-recipe pkg-name))))
               (message "(PatchedSpacemacs.Debug) result=%S" result)
               result)
             (let ((result (progn
                             (message "(PatchedSpacemacs.Debug) action=install-from-elpa")
                             (configuration-layer//install-from-elpa pkg-name))))
               (message "(PatchedSpacemacs.Debug) result=%S" result)
               result))
            (when pkg (cfgl-package-set-property pkg :lazy-install nil)))
           ((and (listp location) (eq 'recipe (car location)))
            (let ((result (progn
                            (message "(PatchedSpacemacs.Debug) action=install-from-recipe")
                            (configuration-layer//install-from-recipe pkg))))
              (message "(PatchedSpacemacs.Debug) result=%S" result)
              result)
            (cfgl-package-set-property pkg :lazy-install nil))
           (t
            (message "(PatchedSpacemacs.Debug) action=none")
            (message "(PatchedSpacemacs.Debug) result=none")
            (configuration-layer//warning "Cannot install package %S."
                                          pkg-name)))
        ('error
         (message "(PatchedSpacemacs.Debug) error=%S" err)
         (configuration-layer//error
          (concat "\nAn error occurred while installing %s "
                  "(error: %s)\n") pkg-name err)
         (spacemacs//redisplay))))))
