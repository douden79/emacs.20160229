;;+--------------------------------~-----------------------~-------------------------+
;;|                         Editor's Settings                                        |
;;+--------------------------------~-----------------------~-------------------------+

;;--------------------------------- Encoding
;; Coding System Read File encoding.
(modify-coding-system-alist 'file "\\.*\\'" 'utf-8)
(setq coding-system-for-read 'utf-8)

;;--------------------------------- Font

;;--------------------------------- korean font setting
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                   '("Gulim" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                   '("Gulim" . "iso10646-1"))

(setq face-font-rescale-alist
       '((".*hiragino.*" . 1.0)
	 (".*Gulim.*" . 1.0)))
(set-language-environment "Korean")

;;--------------------------------- Function-args
;; https://github.com/abo-abo/function-args
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(set-default 'semantic-case-fold t)

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;--------------------------------- Project include Path
;; Add Your include path c/c++ project.
;;(semantic-add-system-include "/usr/local/boost_1_54_0/" 'c++-mode)
(semantic-add-system-include "/home/babel/Build/DRV/BcmLinuxDVB/DRV/brcmlinuxdvbvix/" 'c-mode)
(semantic-add-system-include "/home/babel/WORK/DRV/BCMLinuxDVB/BcmLinuxDVB/DRV/unified_20140613" 'c-mode)

;;--------------------------------- Edit
(add-hook 'after-init-hook 'global-company-mode)

;;--------------------------------- Layout for ecb-layout
(setq ecb-tip-of-the-day nil)
;;(ecb-layout-define "babel-custom-layout" left
;;  "This function creates the following layout by babel:"
;;  (ecb-set-speedbar-buffer)
;;  (ecb-split-ver 0.35)
;;  (ecb-set-methods-buffer)
;;  (ecb-split-ver 0.30)
;;  (ecb-set-symboldef-buffer)
;;  (select-window (next-window)))

;;--------------------------------- Dired
;; latest file list
(setq dired-listing-switches "-alhF")

;;--------------------------------- Helm Gtags setting
(require 'helm-config)
(require 'helm-gtags)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;;--------------------------------- Dired+
;;copy another frame command.
;;(setq dired-dwim-any-frame-flag t)
(setq dired-dwim-target t)
;;--------------------------------- ansi-term
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;;--------------------------------- visit-tags-table
(setq visit-tags-table '("/home/babel/Work/Drv/broadcom-arm/release_bolt_v1.10"))
(provide 'core-editor)
