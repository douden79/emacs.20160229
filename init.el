;;
;; babel emacs configuration init.el.
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(setq gnus-select-method '(nntp "news.gmane.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (function-args dired+ igrep hideshowvis vlf use-package sublimity smex org-bullets multiple-cursors magit highlight-symbol highlight-indent-guides hemisu-theme helm-projectile helm-gtags helm-cscope helm-bind-key flycheck ecb bm better-defaults beacon auto-highlight-symbol atom-one-dark-theme anything)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vlf-application (quote dont-ask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :foundry "fsdf" :full normal :weight normal :height 120 :width normal :full normal :full normal :full normal :full normal :full normal :slant normal)))))

