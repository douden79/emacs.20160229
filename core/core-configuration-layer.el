;; core configuration layer initialization.
(require 'cl-lib)
(require 'package)
(require 'core-load-path)

;; define package archives.
;;
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
