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

;; ▼ Appearance
;; ▼ Editor modify
;; multiple cursor
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
;; ▶ Code Style
;; ▶ Inspections
;; ▶ File Encoding

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
  (editor/hlinum)
  (editor/linum)
  (editor/sublimity)
  (editor/bm)
  (editor/dired+)
  )
