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
 '(compilation-message-face (quote default))
 '(current-language-environment "Korean")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" default)))
 '(ecb-layout-name "left-symboldef" t)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-windows-width 0.25)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(prefer-coding-system (quote utf-8))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Envy Code R VS" :foundry "unknown" :full normal :weight normal :height 98 :width normal :full normal :full normal :full normal :full normal :full normal :full normal :full normal :full normal)))))
