;;
;; emacs first load path
;;

;; emacs personal directory.
(defvar emacs-personal-dir (expand-file-name "personal" emacs-dir)
  "This directory is for your personal configuration.
Users of Emacs are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically.")

;;
;; emacs layer directories.
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

(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-dir)
  "This folder stores all the automatically generated save/history-files.")

(defun emacs/load-path-init ()
  " Defined Directory default init."
  )
(provide 'core-load-path)
