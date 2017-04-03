;;
;; emacs first load path
;;

(defvar emacs-start-directory
  user-emacs-directory
  "emacs start directory.")

(defvar emacs-core-directory
  (expand-file-name (concat emacs-start-directory "core/"))
  "emacs core directory.")

;; ▶ ex) Appearance, VersionControl, Editor, Build, Tools... etc
(defvar emacs-layers-dir (expand-file-name "layers" emacs-dir)
  " This directory is divide to package.")

;; ▶ appearance directory.
(defvar layer-appearance-dir (expand-file-name "+Appearance/packages.el" emacs-layers-dir)
  "This directory Appearance directory.")

;; ▶ VersionControl Directory.
(defvar layer-version-control-dir (expand-file-name "+VersionControl/packages.el" emacs-layers-dir)
  "This Directory VersionControl.")

;; ▶ add layer-version-control-dir
(defun emacs/layer-version-control-path ()
    "emacs version control layer directory load path."
  (load-file layer-version-control-dir)
  (versioncontrol/init))

;; ▶ add theme path init.
(defun emacs/load-appearance-path ()
  "emacs theme layer directory load path."
  (load-file layer-appearance-dir)
  (appearance/init))

(defun emacs/load-path-init ()
  " Defined Directory default init."
  ;; VersionControl init
  (emacs/layer-version-control-path)
  ;; Appearance init
  (emacs/load-appearance-path))

(provide 'core-load-path)
