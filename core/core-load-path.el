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

;; ▶ Editor Directory.
(defvar layer-editor-dir (expand-file-name "+Editor/packages.el" emacs-layers-dir)
  "This Directory Editor.")

;; ▶ add layer-version-control-dir
(defun emacs/layer-version-control-path ()
    "emacs version control layer directory load path."
  (load-file layer-version-control-dir)
  (versioncontrol/init))

;; ▶ add appearance path init.
(defun emacs/load-appearance-path ()
  "emacs theme layer directory load path."
  (load-file layer-appearance-dir)
  (appearance/init))

;; ▶ add editor path init
(defun emacs/load-editor-dir ()
  "emacs editor directory load path."
  (load-file layer-editor-dir)
  (editor/init))

(defun emacs/load-path-init ()
  " Defined Directory default init."
  ;; VersionControl init
  (emacs/layer-version-control-path)
  ;; Appearance init
  (emacs/load-appearance-path)
  ;; Editor init
  (emacs/load-editor-dir))

(provide 'core-load-path)
