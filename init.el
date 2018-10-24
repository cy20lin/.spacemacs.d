;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; TODO:
     ;; check emacs-version before including `treemacs' layer,
     ;; major version should be 25+
     ;; otherwise use `neotree' instead
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               ;; treemacs-use-collapsed-directories 3
               ;; treemacs-use-git-mode 'simple
               )
     ;; text-folding
     lsp
     (my-irony :variables
               ;;
               )
     (my-ide :variables
             my-ide-global-aide-mode-by-default t)
     (c-c++ :variables
            ;; enable clang support if clang is found
            c-c++-enable-irony-support (executable-find "irony-server")
            ;; c-c++-enable-clang-support (executable-find "clang")
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-c++11 t
            ;; c-c++-enable-rtags-support (executable-find "rdm")
            c-c++-enable-rtags-support nil
            )
     ;; dependencies: cmake
     cmake
     ;; dependencies: cquery
     ,(when (executable-find "cquery") 'cquery)
     emacs-lisp
     ;; * Setup javascript layer
     ;; pacboy -S nodejs:x npm:x
     ;; npm install -g tern js-beautify eslint
     (javascript :variables
                 javascript-disable-tern-port-files nil
                 ;; tern-command '("node" "/path/to/tern/bin/tern")
                 js2-basic-offset 4
                 js-indent-level 4
                 ;; (setq-default js2-basic-offset 2)
                 ;; (setq-default js-indent-level 2)
                 )
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            )
     ;; go
     (python :variables
             ;; python-shell-interpreter ,(if (executable-find "python3") "python3" "python")
             ;; flycheck-python-pycompile-executable "python3"
             python-backend 'lsp
             )
     yaml
     ;; haskell
     html
     ;; windows-scripts
     ;; asm
     ;; csv
     ;; latex
     ;; lua
     ;; octave
     ;; restructured-text
     ;; markdown
     org
     ;; +tools/
     ;; pdf-tools
     ;; +intl/
     ;; (chinese :variables chinese-enable-fcitx t)
     ;; +tags/
     ;; cscope
     ;; NOTE: maybe make gtags layer be depended by my-c-c++ layer
     ;; ,(when (executable-find "gtags") 'gtags)
     ;; +syn
     helm
     ;; +emacs/
     better-defaults
     ;; +checkers/
     ;; spell-checking
     (syntax-checking :variables
                      ;; syntax-checking-enable-tooltips nil
                      )
     ;; +compilation/
     (auto-completion :variables
                      ;; auto-completion-enable-help-tooltip t
                      ;; auto-completion-enable-sort-by-usage t
                      ;; auto-completion-return-key-behavior nil            ; 'complete , nil
                      ;; auto-completion-tab-key-behavior 'complete         ; 'complete , 'cycle , nil
                      ;; auto-completion-complete-with-key-sequence nil
                      ;; auto-completion-complete-with-key-sequence-delay 0.1
                      ;; auto-completion-private-snippets-directory nil
                      )
     ;; +fun/
     ;; games
     ;; +source-control/
     git
     ;; version-control
     ;; ----------------------------------------------------------------
     ;; private layers here
     ;; ----------------------------------------------------------------
     ;; (colors :variables
     ;;         colors-enable-rainbow-identifiers t
     ;;         colors-enable-nyan-cat-progress-bar t
     ;;         )
     (my-colors :variables
                my-colors-enable-rainbow-identifiers t
                my-colors-enable-nyan-cat-progress-bar t
                )
     my-icons
     ;; my-c-c++
     ;; (my-blog :variables
     ;;          blog-admin-backend-type 'hexo
     ;;          blog-admin-backend-new-post-in-drafts t
     ;;          blog-admin-backend-new-post-with-same-name-dir t
     ;;          blog-admin-backend-path "~/blog"
     ;;          )
     ,(when (getenv "MSYSTEM") 'my-msystem)
     ;; my-rtags

     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(ox-rst)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; dotspacemacs-startup-lists '((recents . 5)
   ;;                              (projects . 7))
   dotspacemacs-startup-lists '(projects recents bookmarks agenda todos)

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(darkokai
                         spacemacs-dark
                         spacemacs-light
                         monokai
                         zenburn
                         solarized-dark
                         solarized-light
                         leuven
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator nil :separator-scale 1)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 13
   ;;                             :weight normal
   ;;                             :width normal)
   dotspacemacs-default-font '("Source Code Pro"
                               ;; :size 13
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1
                               ;; :powerline-scale 1
                               )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Set custom-file to custom.el to avoid this init.el be populated by
  ;; auto generated custom variable configs.
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  ;; Load configs files in .spacemacs.d/config/<config-dir>/
  (when (and (eq system-type 'windows-nt) (getenv "MSYSTEM"))
    (let ((tmp (getenv "TMPDIR")))
      (when tmp
        (setq-default temporary-file-directory tmp)
        (message "(User) temporary-file-directory set to: %s" temporary-file-directory)
        )))
  (let ((init-files
         ;; Add your init files here (relative to `dotspacemacs-directory')
         '(
           "configs/patch/init.el"
           )))
    (dolist (file init-files)
      (load-file (expand-file-name file dotspacemacs-directory))))
  ;; get modeline height, ref:
  ;; https://stackoverflow.com/questions/9613026/how-to-get-the-height-of-the-emacs-mode-line
  ;; https://stackoverflow.com/questions/9613026/how-to-get-the-height-of-the-emacs-mode-line
  ;; (- (elt (window-pixel-edges) 3) (elt (window-inside-pixel-edges) 3))
  ;; (spaceline-toggle-hud-off)
  ;; Set line padding to 1, same as other line in editing area
  (setq darkokai-mode-line-padding 1)
  ;; Don't use hud now, it will make the height of mode-line inconsistent
  (setq-default powerline-display-hud nil)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; Load configs files in .spacemacs.d/config/<config-dir>/
  (let ((config-files
         ;; Add your config files here (relative to `dotspacemacs-directory')
         '(
           "configs/patch/config.el"
           )))
    (dolist (file config-files)
      (load-file (expand-file-name file dotspacemacs-directory))))
  ;; Customize the powerline separator. Possible values are
  ;; `alternate', `arrow', `arrow-fade', `bar', `box', `brace',
  ;; `butt', `chamfer', `contour', `curve', `curve', `rounded',
  ;; `rounded', `roundstub', `slant', `wave', `zigzag', `nil'
  (setq powerline-default-separator nil)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-off)
  ;; (setq nyan-bar-length 32)
  (setq-default python-shell-interpreter
                (if (executable-find "python3") "python3" "python"))
  ;; Enable global flycheck mode if you need it
  (global-flycheck-mode)
  (global-company-mode)
  ;; Enable caching for projectile porject,
  ;; it default value is nil in Windows,
  ;; so enable it no matter what OS we use.
  (setq projectile-enable-caching t)
  ;; Make sure that the weekdays in the
  ;; time stamps of your Org mode files and
  ;; in the agenda appear in English.
  (setq system-time-locale "C")
  ;;
  (setq warning-minimum-level :error)
  ;; declare coustom leader key
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/declare-prefix "mo" "custom")
  ;;
  (when (configuration-layer/package-usedp 'helm)
    (spacemacs/set-leader-keys "ps" 'helm-multi-swoop-projectile))
  ;;
  (defun my-switch-to-message-buffer ()
    (interactive)
    (spacemacs/goto-buffer-workspace "*Messages*"))
  (spacemacs/set-leader-keys "bM" 'my-switch-to-message-buffer)
  ;;
  (defun my-view-in-chrome ()
    (interactive)
    (process-lines "chromix-too" "open" (concat "file://" (buffer-file-name)))
    )
  (spacemacs/set-leader-keys "oc" 'my-view-in-chrome)
  (defun my-view-doc ()
    (interactive)
    (let* ((default-file (if (projectile-project-p) (concat (projectile-project-root) "doc/_build/html/index.js")))
           (file (if (and default-file (file-exists-p default-file)) default-file (buffer-file-name)))
           (url (concat "file://" file)))
      (process-lines "chromix-too" "open" url)
      ))
  (spacemacs/set-leader-keys "ov" 'my-view-doc)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (c++-mode  . "stroustrup")
                          (c-mode    . "k&r")
                          (other     . "gnu")))
  ;; (modern-c++-font-lock-global-mode t)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (c-set-offset 'innamespace 0)
  ;; in layer c-c++
  ;; spacemacs//c-toggle-auto-newline
  ;; (c-toggle-auto-newline 1)
  ;; (setq projectile-project-compilation-dir "build")
  (spaceline-toggle-hud-off)
  (with-eval-after-load 'org
    ;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode
    (setq
     org-expiry-created-property-name "CREATED" ; Name of property when an item is created
     org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
     )
    (org-expiry-insinuate)
    )
  ;; allow mouse
  (setq helm-allow-mouse t)
  )
