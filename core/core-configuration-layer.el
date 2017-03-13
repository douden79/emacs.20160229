;; core configuration layer initialization.
(require 'cl-lib)
(require 'package)

;; define package archives.
;;
(defvar configuration-layer-elpa-archives
  '(("melpa" . "melpa.org/packages/")
    ("org" . "orgmode.org/elpa/")
    ("gnu" . "elpa.gnu.org/packages/")))

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
  
(defun emacs/configuration-layer-init ()
  "The layer package load path and configuration"
  )

(provide 'core-configuration-layer)
