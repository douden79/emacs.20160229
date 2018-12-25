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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (material-light)))
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "621595cbf6c622556432e881945dda779528e48bb57107b65d428e61a8bb7955" "2eade8d22ce8d204b074c42bc1b22fabccc7e8dd0bbdf47e0a233d1c619a6e7b" "d70c11f5a2b69a77f9d56eff42090138721d4c51d9d39ce986680786d694f492" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default)))
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (termbright-theme white-sand-theme leuven-theme function-args dired+ igrep hideshowvis vlf use-package sublimity smex org-bullets multiple-cursors magit highlight-symbol highlight-indent-guides hemisu-theme helm-projectile helm-gtags helm-cscope helm-bind-key flycheck ecb bm better-defaults beacon auto-highlight-symbol atom-one-dark-theme anything)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vlf-application (quote dont-ask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Pragmata Pro Mono" :foundry "fsdf" :full normal :weight normal :height 90 :width normal :full normal :full normal :full normal :full normal :full normal :slant normal)))))

