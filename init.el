;;
;; babel emacs configuration init.el.
;;

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold 100000000)

(when (version< emacs-version "24.5")
	(error "babel requires at least GNU Emacs 24.5"))
(defvar emacs-dir (file-name-directory load-file-name)
	"The root dir of the Emacs babel distribution.")
(defvar emacs-core-dir (expand-file-name "core" emacs-dir)
  "The home of babel's core functionality.")

;; hide the startup message
(setq inhibit-startup-message t)

;; add babel's directories to Emacs's `load-path'
(add-to-list 'load-path emacs-core-dir)
;;(add-to-list 'load-path babel-modules-dir)

;; the core stuff
(require 'core-emacs)
(require 'core-load-path)
(require 'core-packages)
(require 'core-gui)
(require 'core-editor)
(require 'core-keybindings)
(require 'core-autocomplete)
(require 'core-c++)
(require 'core-ide)
(require 'core-error)
(require 'core-python-mode)
