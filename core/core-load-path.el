;;
;; emacs first load path
;;

(defvar emacs-modules-dir (expand-file-name  "modules" emacs-dir)
  "This directory houses all of the built-in babel modules.")
(defvar emacs-personal-dir (expand-file-name "personal" emacs-dir)
  "This directory is for your personal configuration.
Users of Emacs babel are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by babel.")
(defvar emacs-layers-dir (expand-file-name "layers" emacs-dir)
  " This directory is divide to package.")
(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar emacs-modules-file (expand-file-name "babel-modules.el" emacs-dir)
  "This files contains a list of modules that will be loaded by babel.")
(provide 'core-load-path)
