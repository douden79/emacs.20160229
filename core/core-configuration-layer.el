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

;; refer spacemacs
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

(defun configuration-layer/initialize ()
  "The layer package load path and configuration"
  (setq package-archives (configuration-layer/resolve-package-archives
                          configuration-layer-elpa-archives)))

(provide 'core-configuration-layer)
