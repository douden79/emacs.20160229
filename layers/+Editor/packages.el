;;; packages.el --- smex Layer packages File for babel.
(defun better/smex ()
  "smex use-package initialize."
  (use-package smex
    :ensure t
    :bind
    ("M-x" . smex)
    ("C-f" . helm-imenu)))

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
;; ▼ Company mode
(defun editor/company ()
  "Company mode"
  (use-package company
    :ensure t
    :init (add-hook 'after-init-hook 'global-company-mode)
    )
  )

;; toggle-fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

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

;; python env settings
;; sudo apt-get install python-pip
;; pip install virtualenv
;; pip install sexpdata
;; pip install epc
;; x-x jedi:install-server
(defun editor/python ()
  "install python env"
  (use-package company-jedi
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-jedi))
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--simple-prompt -i")
    (setq python-shell-completion-native-enable nil)
    (add-hook 'python-mode-hook 'jedi:setup))
  )

;; ▼jedi:server-install virtualenv does not exist
(defun editor/exec-path-from-shell ()
  "install exec-path-from-shell."
  (use-package exec-path-from-shell
    :ensure t
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

;; ▼elpy
(defun editor/elpy ()
  "install elpy for python"
  (use-package elpy
    :ensure t
    )
  )

;; 

;; ▼emacs console mode keybindings.
(defun editor/nw-keybindings ()
  "nw keybindings"
  ;; Meta keys
  (bind-key "≈" 'smex)

  ;; bm-toggle
  (bind-key "£" 'bm-toggle)
  (bind-key "™" 'bm-next)
  (bind-key "¡" 'bm-previous)

  ;; helm-gtags
  (bind-key "†" 'helm-gtags-pop-stack)
  (bind-key "‘" 'helm-gtags-find-tag)
  (bind-key "“" 'helm-gtags-find-rtag)
  (bind-key "≥" 'helm-gtags-dwim)
  (bind-key "≤" 'helm-gtags-tags-in-this-function)
  )

;; ▼emacs ecb-keybindings.
(defun editor/ecb-keybindings ()
  "ecb keybindings"

  (bind-key "M-1" 'ecb-goto-window-sources)
  (bind-key "M-2" 'ecb-goto-window-methods)
  (bind-key "M-3" 'ecb-goto-window-symboldef)
  (bind-key "M-4" 'ecb-goto-window-edit1))

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

;; ▼ highlighting inc and bb file.
(defun editor/etc-highlight ()
  "etc-highlight"
  (setq auto-mode-alist
        (cons '("\\.mak\\'" . makefile-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("\\.bb\\'" . makefile-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("\\.inc\\'" . makefile-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("\\.conf\\'" . makefile-mode) auto-mode-alist))
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
           ("C-;" . mc/mark-all-like-this)
           ("C-'" . mc/mark-all-words-like-this)))
  )

;; search igrep
(defun editor/igrep ()
  "igrep init"
  (use-package igrep
    :ensure t
    :bind (("C-s" . rgrep))
    )
  )

;; hlinum
(defun editor/hlinum ()
  "hlinum init"
  (use-package hlinum
    :ensure t
    :config
    (global-linum-mode t)
    (defun linum-update-window-scale-fix (win)
      "fix linum for scaled text"
      (set-window-margins win
                          (ceiling (* (if (boundp 'text-scale-mode-step)
                                          (expt text-scale-mode-step
                                                text-scale-mode-amount) 1)
                                      (if (car (window-margins))
                                          (car (window-margins)) 1)))))
    (advice-add #'linum-update-window :after #'linum-update-window-scale-fix))
  )

;; linum
(defun editor/linum ()
  "linum init"
  (use-package linum
    :ensure t
    :config
    (global-hl-line-mode +1)
    (setq linum-format "%-4d"))
  (column-number-mode t)
  (size-indication-mode t)
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

;; markdown
(defun editor/markdown ()
  "markdown mode"
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
            ("\\.md\\'" . markdown-mode)
            ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))
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
           ([f4] . highlight-symbol-remove-all)
           ("M-<left>" . highlight-symbol-next)
           ("M-<right>" . highlight-symbol-prev))
    )
  )

;; auto-highlight-symbol
(defun editor/auto-highlight-symbol ()
  "thils only installs it for programming mode derivatives. you can also makeit global.."
  (use-package auto-highlight-symbol
    :ensure t
    :init (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (global-auto-highlight-symbol-mode t)
    :bind (:map auto-highlight-symbol-mode-map
                ("M-p" . ahs-backward)
                ("M-n" . ahs-forward)
                )
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
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; gtags setting
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    )
  )

(defun editor/autopair ()
  "autopair"
  (use-package autopair)
  :ensure t
  :init (add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
  )

;; Helm cscope
(defun editor/helm-cscope ()
  "Helm cscope settings."
  (use-package helm-cscope
    :ensure t
    :init
    (bind-key "M-c" 'helm-cscope-find-calling-this-function)
    (bind-key "M-]" 'helm-cscope-find-global-definition)
    (bind-key "M-[" 'helm-cscope-pop-mark)
    )
  )
 

;; helm swoop
(defun editor/helm-swoop ()
  "Helm swoop setting and helm-imenu."
  (use-package helm-swoop
    :ensure t
    :bind (("C-c o" . helm-swoop)
           ("C-c O" . helm-multi-swoop)))
  )

;; helm-bind-key
(defun editor/helm-bind-key ()
  "provides helm interface for bind-key. making customize key binding in helm"
  (use-package helm-bind-key
    :ensure t
    ;; Not implimantation in helm bind key.
    )
  )

;; ▼ ECB
(defun editor/ecb ()
  "ECB IDE init"
  (use-package ecb
    :ensure t)
  :init (setq ecb-layout-name "left-symboldef")
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
  (setq indent-tabs-mode nil) 
  (setq c-basic-offset 8)
  (add-hook 'c-mode-hook 'linux-c-indent)
  (add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
  (add-hook 'c++-mode-hook 'linux-c-indent)
  )
;; nlinum
(defun editor/nlinum ()
  "nlinum init"
  (global-nlinum-mode t)
  (setq nlinum-format "%4d")
  (global-hl-line-mode 1)
;;  (global-hl-line-highlight)
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
(defun my-font-lock-function (start end)
  "Set faces for font-lock between START and END.")

(defun my-font-lock-matcher (limit)
  (my-font-lock-function (point) limit)
  nil)

;; editor general settings.
(defun editor/general ()
  "editor general init"
  (add-hook 'c-mode-hook 'linux-c-indent)
  (add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
  (add-hook 'c++-mode-hook 'linux-c-indent)

  (setq dired-dwim-target t)
  (semantic-mode t)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

;;  (global-semantic-idle-scheduler-mode t)
;;  (global-semanticdb-minor-mode t)
;;  (global-semantic-idle-summary-mode t)
;;  (global-semantic-idle-completions-mode t)
  (global-semantic-highlight-func-mode t)
  (global-semantic-decoration-mode t)
  (global-semantic-stickyfunc-mode t)
  (global-semantic-mru-bookmark-mode t)
  (setq-default semantic-symref-tool "cscope")

  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'python-mode)

  (add-hook 'term-setup-hook
  '(lambda ()
     (define-key function-key-map "\e[1;9A" [M-up])
     (define-key function-key-map "\e[1;9B" [M-down])
     (define-key function-key-map "\e[1;9C" [M-right])
     (define-key function-key-map "\e[1;9D" [M-left])))

  ;;  large file open warning popup.
  (setq large-file-warning-threshold nil)


  ;; Improved Font Lock Speed
  (setq font-lock-defaults
        (list
         "Font lock speed improved"
         `(my-font-lock-matcher (1 font-lock-keyword-face nil))))

  ;;  (setq jit-lock-stealth-time 10)

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  )

;; This is a function copied from stackoverflow to facify #if 0/#else/#endif keywords.
;; The comments are added by myself to make it understandable. 
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
	;; Search #if/#else/#endif using regular expression.
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
	  ;; Handle #if.
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
		;; Handle neariest 0.
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
	    ;; Handle #else, here we can decorate #if 0->#else block using 'font-lock-comment-face'.
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
	    ;; Handle #endif, return to upper block if possible.
            (when (string= str "endif")
              (setq depth (1- depth)))))
	;; Corner case when there are only #if 0 (May be you are coding now:))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)
(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))


;; Editor init
(defun editor/init ()
  "Editor envirment init"
  ;; Default init
  (better/smex)
  
  ;; Autocomplete
  ;;(editor/autocomplete)
  ;;(editor/yasnippet)

  ;; code folding
  ;;(editor/hideshowvis)

  ;; Editor modify
  (editor/igrep)
  (editor/multiplecursor)

  ;; Hlinum
  (editor/sublimity)
  (editor/bm)

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
  (editor/auto-highlight-symbol)
;;  (editor/markdown)

  ;; font
  (editor/font)

  ;; company
  ;; nlinum
  (editor/nlinum)
  (editor/etc-highlight)

  ;; emacs console mode keybindings
  (editor/nw-keybindings)
  (editor/ecb-keybindings)

  ;; helm bind-key
  (editor/helm-bind-key)
  (editor/helm-cscope)

  ;; python settings
  (editor/python)
;;  (editor/elpy)
  (editor/exec-path-from-shell)
  )
