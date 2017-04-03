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
(require 'core-packages)
(emacs/init)

;;▶
;;▷
;;▒
;;▒
;;▼
;;◎

;; TODO :
;;(configuration-layer/sync)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Envy Code R" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
