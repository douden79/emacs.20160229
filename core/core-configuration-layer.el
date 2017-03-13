;; core configuration layer initialization.
(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'package)
(require 'core-load-path)
(require 'ht)

;;
;; ht-get

(defun ht-get (table key &optional default)
  "Look up KEY in TABLE, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (gethash key table default))

(defun ht-contains? (table key)
  "Return 't if TABLE contains KEY."
  (not (eq (ht-get table key 'ht--not-found) 'ht--not-found)))

;;
;; dotemacs configuration define.
;;
(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load.")

(defvar dotspacemacs-elpa-subdirectory nil
  "If non-nil, a form that evaluates to a package directory. For
example, to use different package directories for different Emacs
versions, set this to `emacs-version'.")

(defvar configuration-layer--load-packages-files nil
  "If non-nil force loading `packages.el' files when creating layer objects.")

(defvar configuration-layer-exclude-all-layers nil
  "If non nil then only the distribution layer is loaded.")

(defvar dotspacemacs-install-packages 'used-only
  "Defines the behaviour of Spacemacs when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and uninstall any unused packages as well
as their unused dependencies. `used-but-keep-unused' installs only the used
packages but won't uninstall them if they become unused. `all' installs *all*
packages supported by Spacemacs and never uninstall them.")

(defun configuration-layer/get-layer (layer-name)
  "Return a layer object with name LAYER-NAME.
Return nil if layer object is not found."
  (when (ht-contains? configuration-layer--indexed-layers layer-name)
    (ht-get configuration-layer--indexed-layers layer-name)))

(let* ((env (getenv "EMACS"))
       (env-dir (when env (expand-file-name (concat env "/"))))
       (env-init (and env-dir (expand-file-name "init.el" env-dir)))
       (no-env-dir-default (expand-file-name
                            (concat user-home-directory
                                    ".emacs.d/")))
       (default-init (expand-file-name ".emacs.d" user-home-directory)))
  (defconst dotspacemacs-directory
    (cond
     ((and env (file-exists-p env-dir))
      env-dir)
     ((file-exists-p no-env-dir-default)
      no-env-dir-default)
     (t
      nil))
    "Optional spacemacs directory, which defaults to
~/.spacemacs.d. This setting can be overridden using the
SPACEMACSDIR environment variable. If neither of these
directories exist, this variable will be nil.")

  (defvar dotspacemacs-filepath
    (let ((spacemacs-dir-init (when dotspacemacs-directory
                                 (concat dotspacemacs-directory
                                         "init.el"))))
      (cond
       (env-init)
       ((file-exists-p default-init) default-init)
       ((and dotspacemacs-directory (file-exists-p spacemacs-dir-init)) spacemacs-dir-init)
       (t default-init)))
    "Filepath to the installed dotfile. If SPACEMACSDIR is given
then SPACEMACSDIR/init.el is used. Otherwise, if ~/.spacemacs
exists, then this is used. If ~/.spacemacs does not exist, then
check for init.el in dotspacemacs-directory and use this if it
exists. Otherwise, fallback to ~/.spacemacs"))

;; refer spacemacs
(defconst configuration-layer-private-layer-directory
  (let ((dotspacemacs-layer-dir
         (when dotspacemacs-directory
           (expand-file-name
            (concat dotspacemacs-directory "layers/")))))
    (if (and dotspacemacs-directory
             (file-exists-p dotspacemacs-layer-dir))
        dotspacemacs-layer-dir
      configuration-layer-private-directory))
  "Spacemacs default directory for private layers.")

;; add layer
(defun configuration-layer/add-layer (layer &optional usedp)
  "Add a LAYER object to the system.
USEDP non-nil means that PKG is a used layer."
  (let ((layer-name (oref layer :name)))
    (puthash layer-name layer configuration-layer--indexed-layers)
    (when usedp
      (add-to-list 'configuration-layer--used-layers layer-name))))

;; make package
(defun configuration-layer/make-layer (layer-specs &optional obj usedp dir)
  "Return a `cfgl-layer' object based on LAYER-SPECS.
If LOAD-PKGS is non-nil then load the `packages.el' file of the layer.
DIR is the directory where the layer is, if it is nil then search in the
indexed layers for the path."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (obj (if obj obj (cfgl-layer (symbol-name layer-name)
                                      :name layer-name)))
         (dir (or dir (oref obj :dir))))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
;;        (configuration-layer//warning
;;         "Cannot make layer %S without a valid directory!"
;;         layer-name)
      (let* ((dir (file-name-as-directory dir))
             (disabled (when (listp layer-specs)
                         (spacemacs/mplist-get layer-specs :disabled-for)))
             (enabled (if (and (listp layer-specs)
                               (memq :enabled-for layer-specs))
                          (spacemacs/mplist-get layer-specs :enabled-for)
                        'unspecified))
             (variables (when (listp layer-specs)
                          (spacemacs/mplist-get layer-specs :variables)))
             (packages-file (concat dir "packages.el"))
             (packages
              (if (and (or usedp configuration-layer--load-packages-files)
                       (file-exists-p packages-file))
                  (progn
                    (load packages-file)
                    (symbol-value (intern (format "%S-packages" layer-name))))
                (oref obj :packages)))
             (selected-packages (if packages
                                    (configuration-layer//select-packages
                                     layer-specs packages)
                                  ;; default value
                                  'all)))
        (oset obj :dir dir)
        (when usedp
          (oset obj :disabled-for disabled)
          (oset obj :enabled-for enabled)
          (oset obj :variables variables))
        (when packages
          (oset obj :packages packages)
          (oset obj :selected-packages selected-packages))
        obj))))

(defclass cfgl-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :initform nil
        :type (satisfies (lambda (x) (or (null x) (stringp x))))
        :documentation "Absolute path to the layer directory.")
   (packages :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
   (selected-packages :initarg :selected-packages
             :initform 'all
             :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x))
                                              (listp x))))
             :documentation "List of selected package symbols.")
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the layer needs to be installed")
   (disabled :initarg :disabled-for
             :initform nil
             :type list
             :documentation "A list of layers where this layer is disabled.")
   (enabled :initarg :enabled-for
            :initform 'unspecified
            :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
            :documentation (concat "A list of layers where this layer is enabled. "
                                   "(Takes precedence over `:disabled-for'.)")))
  "A configuration layer.")


;; define package archives.
(defvar configuration-layer-elpa-archives
  '(("melpa" . "melpa.org/packages/")
    ("org" . "orgmode.org/elpa/")
    ("gnu" . "elpa.gnu.org/packages/"))
  "List of ELPA carhives required.")

;; TODO : refer to space emacs configuration-layer lazy-install-packages --> layer/sync -->
;; auto-mode --> lazy-install --> insert-lazy-install-form --> insert-lazy-install-configuration
;;
;; refer to spacemacs layer/sync configuration-layer/sync is called in init.el file.
;;

;; default configuration layer directory
;;
(defconst configuration-layer-directory
  (expand-file-name (concat emacs-start-directory "layers/"))
  "Emacs contribution layers base directory")

(defun configuration-layer/directory-type (path)
  "Return the type of directory pointed by PATH"
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat configuration-layer-directory path))))
        'category
      (let ((files (directory-files path)))
        (when (or (member "packages.el" files)
                  (member "layers.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

;; refer to spacemacs
(defun configuration-layer/resolve-package-archives (archives)
  "Resolve HTTP handlers for each archive in ARCHIVES and return a list
of all reachable ones.
If the address of an archive already contains the protocol then this address is
left untouched.
The returned list has a `package-archives' compliant format."
  (mapcar
   (lambda (x)
     (cons (car x)
           (if (or (string-match-p "http" (cdr x))
                   (string-prefix-p "/" (cdr x)))
               (cdr x))))
   archives))

(defun configuration-layer/get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `+'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat configuration-layer-directory
                             dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defun configuration-layer/warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'.
If `configuration-layer--inhibit-warnings' is non nil then this function is a
no-op."
  (unless configuration-layer--inhibit-warnings
    ;;(apply 'spacemacs-buffer/warning msg args)
    ))

(defvar configuration-layer--inhibit-warnings nil
  "If non-nil then warning message emitted by the layer system are ignored.")

;; discover layer
(defun configuration-layer/discover-layers ()
  "Initialize `configuration-layer--indexed-layers' with layer directories."
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (setq  configuration-layer--indexed-layers (make-hash-table :size 1024))
  (let ((search-paths (append (list configuration-layer-directory)
                              dotspacemacs-configuration-layer-path
                              (list configuration-layer-private-layer-directory)
                              (when dotspacemacs-directory
                                (list dotspacemacs-directory))))
        (discovered '()))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (configuration-layer/directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (configuration-layer/get-category-from-path
                                 sub)))
;;                  (spacemacs-buffer/message "-> Discovered category: %S"
;;                                            category)
                  (push category configuration-layer-categories)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'layer type)
                (let* ((layer-name-str (file-name-nondirectory sub))
                       (layer-name (intern layer-name-str))
                       (indexed-layer (configuration-layer/get-layer
                                       layer-name)))
                  (if indexed-layer
                      ;; the same layer may have been discovered twice,
                      ;; in which case we don't need a warning
                      (unless (string-equal (oref indexed-layer :dir) sub)
                        (configuration-layer/warning
                         (concat
                          "Duplicated layer %s detected in directory \"%s\", "
                          "replacing old directory \"%s\" with new directory.")
                         layer-name-str sub (oref indexed-layer :dir))
                        (oset indexed-layer :dir sub))
;;                    (spacemacs-buffer/message
;;                     "-> Discovered configuration layer: %s" layer-name-str)
                    (configuration-layer/add-layer
                     (configuration-layer/make-layer layer-name nil nil sub)))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))

(defun configuration-layer//declare-used-layers (layers-specs)
  "Declare used layers from LAYERS-SPECS list."
  (setq configuration-layer--used-layers nil)
  (let ((configuration-layer--declared-layers-usedp t))
    (unless configuration-layer-exclude-all-layers
      (dolist (layer-specs layers-specs)
        (let* ((layer-name (if (listp layer-specs)
                               (car layer-specs)
                             layer-specs))
               (layer (configuration-layer/get-layer layer-name)))
          (if layer
              (let ((layer-path (oref layer :dir)))
                (unless (string-match-p "+distributions" layer-path)
                  (configuration-layer/declare-layer layer-specs)))
;;            (configuration-layer//warning
;;             "Unknown layer %s declared in dotfile." layer-name)
            )))
      (setq configuration-layer--used-layers
            (reverse configuration-layer--used-layers)))
    ;; distribution and bootstrap layers are always first
    (let ((distribution (if configuration-layer-force-distribution
                            configuration-layer-force-distribution
                          dotspacemacs-distribution)))
      (unless (eq 'spacemacs-bootstrap distribution)
        (configuration-layer/declare-layer distribution)))
    (configuration-layer/declare-layer 'spacemacs-bootstrap)))

;; refer to spacemacs sync.
;;
(defun configuration-layer/sync (&optional no-install)
  "Synchronize declared layers in dotfile with spacemacs.
If NO-INSTALL is non nil then install steps are skipped."
;;  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
;;  (setq dotspacemacs--configuration-layers-saved
;;        dotspacemacs-configuration-layers)

  ;; declare used layers then packages as soon as possible to resolve
  ;; usage and ownership
  (configuration-layer/discover-layers)
  (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
  (configuration-layer//declare-used-packages configuration-layer--used-layers)
  ;; then load the functions and finally configure the layers
  (configuration-layer//load-layers-files configuration-layer--used-layers
                                          '("funcs.el"))
  (configuration-layer//configure-layers configuration-layer--used-layers)
  ;; pre-filter some packages to save some time later in the loading process
  (setq configuration-layer--used-distant-packages
        (configuration-layer//get-distant-packages
         configuration-layer--used-packages t))
  ;; load layers lazy settings
  (configuration-layer/load-auto-layer-file)
  ;; install and/or uninstall packages
  (unless no-install
    (let ((packages
           (append
            ;; install used packages
            (configuration-layer/filter-objects
             configuration-layer--used-distant-packages
             (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (not (oref pkg :lazy-install)))))
            ;; also install all other packages if requested
            (when (eq 'all dotspacemacs-install-packages)
              (let (all-other-packages)
                (dolist (layer (configuration-layer/get-layers-list))
                  (let ((configuration-layer--declared-layers-usedp nil)
                        (configuration-layer--load-packages-files t))
                    (configuration-layer/declare-layer layer)
                    (let* ((obj (configuration-layer/get-layer layer))
                           (pkgs (when obj (oref obj :packages))))
                      (configuration-layer/make-packages-from-layers
                       (list layer))
                      (dolist (pkg pkgs)
                        (let ((pkg-name (if (listp pkg) (car pkg) pkg)))
                          (add-to-list 'all-other-packages pkg-name))))))
                (configuration-layer//get-distant-packages
                 all-other-packages nil))))))
      (configuration-layer//install-packages packages)
      (when (and (or (eq 'used dotspacemacs-install-packages)
                     (eq 'used-only dotspacemacs-install-packages))
                 (not configuration-layer-force-distribution)
                 (not configuration-layer-exclude-all-layers))
        (configuration-layer/delete-orphan-packages packages))))
  ;; configure used packages
  (configuration-layer//configure-packages configuration-layer--used-packages)
  (configuration-layer//load-layers-files configuration-layer--used-layers
                                          '("keybindings.el")))

(defun configuration-layer/initialize ()
  "The layer package load path and configuration"
  (setq package-archives (configuration-layer/resolve-package-archives
                          configuration-layer-elpa-archives)))

(provide 'core-configuration-layer)
