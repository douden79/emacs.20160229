;; core configuration layer initialization.
(require 'cl-lib)
(require 'package)
(require 'core-load-path)

;;
;; dotemacs configuration define.
;;
(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-elpa-subdirectory nil
  "If non-nil, a form that evaluates to a package directory. For
example, to use different package directories for different Emacs
versions, set this to `emacs-version'.")

(defvar dotspacemacs-install-packages 'used-only
  "Defines the behaviour of Spacemacs when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and uninstall any unused packages as well
as their unused dependencies. `used-but-keep-unused' installs only the used
packages but won't uninstall them if they become unused. `all' installs *all*
packages supported by Spacemacs and never uninstall them.")

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
            (let ((type (configuration-layer//directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (configuration-layer//get-category-from-path
                                 sub)))
                  (spacemacs-buffer/message "-> Discovered category: %S"
                                            category)
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
                        (configuration-layer//warning
                         (concat
                          "Duplicated layer %s detected in directory \"%s\", "
                          "replacing old directory \"%s\" with new directory.")
                         layer-name-str sub (oref indexed-layer :dir))
                        (oset indexed-layer :dir sub))
                    (spacemacs-buffer/message
                     "-> Discovered configuration layer: %s" layer-name-str)
                    (configuration-layer//add-layer
                     (configuration-layer/make-layer layer-name nil nil sub)))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))

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
