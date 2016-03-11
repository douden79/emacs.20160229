;;-------------------------~-----------------------~--------------------------------
;;;	babel's configuration entry point.
;;--------------------------------~-----------------------~-------------------------
(when (version< emacs-version "24.5")
	(error "babel requires at least GNU Emacs 24.5"))
(defvar babel-dir (file-name-directory load-file-name)
	"The root dir of the Emacs babel distribution.")
(defvar babel-core-dir (expand-file-name "core" babel-dir)
  "The home of babel's core functionality.")
(defvar babel-modules-dir (expand-file-name  "modules" babel-dir)
  "This directory houses all of the built-in babel modules.")
(defvar babel-personal-dir (expand-file-name "personal" babel-dir)
  "This directory is for your personal configuration.
Users of Emacs babel are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by babel.")
(defvar babel-savefile-dir (expand-file-name "savefile" babel-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar babel-modules-file (expand-file-name "babel-modules.el" babel-dir)
  "This files contains a list of modules that will be loaded by babel.")

;;--------------------------------~-----------------------~-------------------------
;;                         Start up Default Setting
;;--------------------------------~-----------------------~-------------------------
(setq inhibit-startup-message t) ;; hide the startup message

;;-------------------------~-------------------------~----------------------------
;;                           emacs module loading
;;-----------------------------~----------------------------~---------------------
;; add babel's directories to Emacs's `load-path'
(add-to-list 'load-path babel-core-dir)
(add-to-list 'load-path babel-modules-dir)
;; the core stuff
(require 'packages)
(require 'gui)
(require 'editor)
(require 'core)
(require 'keybindings)
(require 'autocomplete)
(require 'c++)
(require 'ide)
(require 'error)
(require 'python-mode)

;;+----> Debug Mode enable
;;(setq debug-on-error t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(current-language-environment "Korean")
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" default)))
 '(ecb-layout-name "left-dir-plus-speedbar")
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-windows-width 0.25)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(prefer-coding-system (quote utf-8))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Envy Code R" :foundry "unknown" :full normal :weight normal :height 98 :width normal :full normal :full normal :full normal :full normal :full normal :full normal :full normal :slant normal)))))
