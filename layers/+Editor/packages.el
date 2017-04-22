;;; packages.el --- smex Layer packages File for babel.
(defun better/smex ()
  "smex use-package initialize."
  (use-package smex
    :ensure t
    :bind
    ("M-x" . smex)))

;; ▶ General
;; ▼ CodeComplete ( Autocomplete, yasnippet )
(defun editor/autocomplete ()
  "autocomplete init"
  (use-package auto-complete
    :ensure t
    :init (ac-config-default)
    (global-auto-complete-mode t)
    (setq ac-auto-start 1)
    (setq ac-auto-show-menu 0.1)
    (ac-set-trigger-key "TAB"))
  )

;; ▼ yasnippet
(defun editor/yasnippet ()
  "yasnippet init"
  (use-package yasnippet
    :ensure t
    :defer t
    :diminish yas-minor-mode
    :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
    :init
    (progn
      (setq yas-verbosity 3)
      (yas-global-mode 1))
    (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")
    (add-hook
     'prog-mode-hook
     (lambda ()
       (setq ac-sources
             (append '(ac-source-yasnippet) ac-sources)))))
  )

;; ▼ better default/ido-mode
(defun editor/better-default ()
  "better-default init"
  (use-package better-defaults
    :ensure t
    :init (ido-mode 0))
  )

  ;; ▼ smooth scrolling
(defun editor/smooth-scrolling ()
  "line by line pause improve"
  (setq redisplay-dont-pause t)
  )

;; ▼ function args
(defun editor/function-args ()
  "function-args init"
  (use-package function-args
    :ensure t
    :config (fa-config-default)
             (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
             (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
             (set-default 'semantic-case-fold t))
  )

;; ▶ Appearance

;; ▼ Editor modify
;; ▼ Large file opne
(defun editor/vlf ()
  "Large file open"
  (use-package vlf
    :ensure t
    :config (custom-set-variables '(vlf-application 'dont-ask)))
  )

;; ▼ multiple cursor
(defun editor/multiplecursor ()
  "Multiple Cursor Init"
  (use-package multiple-cursors
    :ensure t
    :bind (("C-l" . mc/edit-lines)
           ("C-;" . mc/mark-all-like-this)))
  )

(use-package multiple-cursors
:ensure t
:bind (("C-l" . mc/edit-lines)
("C-;" . mc/mark-all-like-this)))

;; search igrep
(defun editor/igrep ()
  "igrep init"
  (use-package igrep
    :ensure t
    :bind (("C-s" . rgrep))
    )
  )

;; nlinum
(defun editor/nlinum ()
  "nlinum init"
  (global-nlinum-mode t)
  (setq nlinum-format "%4d")
  )

;; sublimity : smooth scrolling
(defun editor/sublimity ()
  "sublimity init"
  (use-package sublimity
    :ensure t
    :config
    (setq sumlimity-scroll-weight 2
          sublimity-scroll-drift-length 2)
    (setq sublimity-attractive-centering-width 110))
  )

;; dired+
(defun editor/dired+ ()
  "Dired plus"
  (use-package dired+
    :ensure t
    :init (setq dired-dwim-target t))
  )

;; bm bookmark
(defun editor/bm ()
  "Bookmark package"
  (use-package bm
    :ensure t
    :bind (("C-3" . bm-toggle)
           ("C-2" . bm-next)
           ("C-1" . bm-previous)))
  )

;; ▼ code folding
(defun editor/hideshowvis ()
  "hideshowvis init"
  (use-package hideshowvis
    :ensure t
    :bind (("C-t" . hs-toggle-hiding))
    :init (add-hook 'c-mode-hook #'hideshowvis-enable)
    (add-hook 'c++-mode-hook #'hideshowvis-enable)
    (add-hook 'c-mode-hook #'hideshowvis-symbols)
    (add-hook 'c++-mode-hook #'hideshowvis-symbols))
  )

;; ▶ Color / Fonts
;; ▼ highlight-symbol
(defun editor/highlightsymbol ()
  "highlightsymbol"
  (use-package highlight-symbol
    :ensure t
    :bind (([f3] . highlight-symbol-at-point)
           ([f4] . highlight-symbol-remove-all))
    )
  )

;; ▶ IDE
;; ▼ Helm Packages
(defun editor/helm-gtags ()
  "Helm gtags setting."
  (use-package helm-gtags
    :ensure t
    :commands (helm-gtags-mode helm-gtags-dwim)
    :diminish "HGt"
    :bind (("M-t" . helm-gtags-pop-stack)
           ("M-]" . helm-gtags-find-tag)
           ("M-[" . helm-gtags-find-rtag)
           ("M-." . helm-gtags-dwim)
           ("M-," . helm-gtags-tags-in-this-function)
           ("C-j" . helm-gtags-select)
           ("M-g M-p" . helm-gtags-parse-file))
    :init
    ;; Enable helm-gtags-mode in code
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    )
  )
;; helm swoop
(defun editor/helm-swoop ()
  "Helm swoop setting."
(use-package helm-swoop
  :ensure t
  :bind (("C-c o" . helm-swoop)
         ("C-c O" . helm-multi-swoop)))
)

;; ▼ ECB
(defun editor/ecb ()
  "ECB IDE init"
  (use-package ecb
    :ensure t)
  :init (setq ecb-layout-name "right1")
  (setq ecb-examples-bufferinfo-buffer-name nil)
  (setq stack-trace-on-error t)
  (setq ecb-version-check nil)
  (setq ecb-compile-window-height 12)
  )

;; ▼ HELM-Projectile
(defun editor/helm-projectile ()
  "helm-projectile"
  (use-package helm-projectile
    :ensure t
    :bind (("C-p" . helm-projectile))
    :config (helm-projectile-on)
    :init (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-remember-window-configs t)
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'helm-projectile)
    (projectile-global-mode))
  )

;; ▶ Code Style
;; linux c mode
(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use with the Linux kernel."
  (interactive)
  (setq tab-width 8)
  ;;force spaces, to work with dumber editors
  (setq indent-tabs-mode nil) 
  (setq c-basic-offset 8)
  (add-hook 'c-mode-hook 'linux-c-indent)
  (add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
  (add-hook 'c++-mode-hook 'linux-c-indent)
  )

;; ▼ flycheck
(defun editor/flycheck ()
  "flycheck init"
  (use-package flycheck
    :ensure t
    :init (global-flycheck-mode))
  )

;; ▶ Inspections
;; ▶ File Encoding
;; font setting.
(defun editor/font ()
  "font setting"
    ;; default Latin font (e.g. Consolas)
  ;; but I use Monaco 
  (set-face-attribute 'default nil :family "Envy Code R")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 130)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "한겨레결체"))

  ;; you may want to add different for other charset in this way.
  (set-language-environment "Korean")
  )

;; editor etc settings.
(defun editor/etc ()
  "editor settings."
  (fset 'yes-or-no-p 'y-or-n-p)
  (modify-coding-system-alist 'file "\\.*\\'" 'utf-8)
  (setq coding-system-for-read 'utf-8)
  (setq-default
   whitespace-line-column 80
   whitespace-style       '(face lines-tail))
  )

;; editor general settings.
(defun editor/general ()
  "editor general init"
  (add-hook 'c-mode-hook 'linux-c-indent)
  (add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
  (add-hook 'c++-mode-hook 'linux-c-indent)

  (semantic-mode t)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

  (global-semantic-idle-scheduler-mode t)
  (global-semanticdb-minor-mode t)
  (global-semantic-idle-summary-mode t)
  (global-semantic-idle-completions-mode t)
  (global-semantic-highlight-func-mode t)
  (global-semantic-decoration-mode t)
  (global-semantic-stickyfunc-mode t)
  (global-semantic-mru-bookmark-mode t)
  (setq-default semantic-symref-tool "global")

  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'python-mode)
  )

;; Editor init
(defun editor/init ()
  "Editor envirment init"
  ;; Default init
  (better/smex)
  
  ;; Autocomplete
  (editor/autocomplete)
  (editor/yasnippet)

  ;; code folding
  (editor/hideshowvis)

  ;; Editor modify
  (editor/igrep)
  (editor/multiplecursor)

  ;; Hlinum
  (editor/nlinum)
  (editor/sublimity)
  (editor/bm)
  (editor/dired+)

  ;; color/symbol
  (editor/highlightsymbol)

  ;; function
  (editor/function-args)

  ;; file/modify
  (editor/vlf)

  ;; IDE
  (editor/helm-projectile)
  (editor/ecb)
  (editor/helm-gtags)

  ;; Scrolling
  (editor/sublimity)
  (editor/smooth-scrolling)
  (editor/etc)

  ;; code style
  (linux-c-indent)

  ;; editor general
  (editor/general)
  (editor/flycheck)
  (editor/better-default)

  ;; font
  (editor/font)
  )
