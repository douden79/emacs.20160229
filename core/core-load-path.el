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

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-dir)
  "This folder stores all the automatically generated save/history-files.")

(defun emacs/load-path-init ()
  " Defined Directory default init."
  )
(provide 'core-load-path)
