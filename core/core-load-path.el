;;
;; emacs first load path
;;

;;
;; emacs layer directories.

(defvar emacs-start-directory
  user-emacs-directory
  "emacs start directory.")

(defvar emacs-core-directory
  (expand-file-name (concat emacs-start-directory "core/"))
  "emacs core directory.")

;; ex) lang, emacs, theme, tags
(defvar emacs-layers-dir (expand-file-name "layers" emacs-dir)
  " This directory is divide to package.")
;; emacs language directories.
(defvar layer-lang-dir (expand-file-name "+lang" emacs-layers-dir)
  " This directory language directory.")
;; emacs directories.
(defvar layer-emacs-dir (expand-file-name "+emacs" emacs-layers-dir)
  " This directory emacs directory.")
;; theme directories.
(defvar layer-theme-dir (expand-file-name "+theme" emacs-layers-dir)
  " This direcotry theme directory.")
;; tags directories.
(defvar layer-tag-dir (expand-file-name "+tags" emacs-layers-dir)
  " This directory tags directory.")

;; source control directory.
(defvar layer-source-control-dir (expand-file-name "+source-control" emacs-layers-dir)
  "This directory source control directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

;;;; load C/C++ file.
(defvar layer-c-c++dir (expand-file-name "c-c++/packages.el" layer-lang-dir)
  " This directory c-c++ directory.")

;;;; load source control file.
(defvar layer-magit-dir (expand-file-name "magit/packages.el" layer-source-control-dir)
  "This directory source control directory.")

;; add-lang-path
(defun emacs/add-lang-path ()
  "emacs lang layer directory load path."
  (load-file layer-c-c++dir)
  )

;; add source-control-path
(defun emacs/source-contorl-path ()
  "emacs source-control layer directory load path."
  (load-file layer-magit-dir)
  (magit/init))

(defun emacs/load-path-init ()
  " Defined Directory default init."
  ;; language define HERE!!!
  (emacs/add-lang-path)
  ;; source-control define HERE!!!
  (emacs/source-contorl-path)
  )
(provide 'core-load-path)
