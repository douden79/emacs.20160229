;; ▶ THEME : default monokai
(defun theme/monokai-init ()
  "Monokai theme install."
  (use-package monokai-theme
    :ensure t
    :init
    (setq monokai-theme-kit t)
    (load-theme 'monokai t)
    ))

;; ▶ Menu/ToolBar
;; ▶ System Settings

;; ▶ Scopes
;; ▼ beacon
(defun scope/beacon-init ()
  "current line highlight"
  (use-package beacon
    :ensure t
    :init (beacon-mode 1)))

;; ▼ winmove
(defun scope/winmove-init ()
  "window move init"
  (use-package winmove
    :bind (("M-<right>" . windmove-right)
	   ("M-<left>" . windmove-left)
	   ("M-<up>" . windmove-up)
	   ("M-<down>" . windmove-down))
    ))

;; ▼ highlight-indent-guides
(defun scope/highlight-indent-init ()
  "highlight indent guide"
  (use-package highlight-indent-guides
    :ensure t
    :config (add-hook 'c-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'c++-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'shell-script-mode 'highlight-indent-guides-mode)
    :init (setq highlight-indent-guides-method 'character)))

;; ▼ better default
(defun scope/better-default-init ()
  "better default init"
  (use-package better-defaults
    :ensure t
    :init (ido-mode 0))
  )

;; ▼ anything mode
(defun scope/anything-init ()
  "anything init"
  (use-package anything
    :ensure t
    :bind (("C-b" . anything-mini))
    ))

;; Appearance Init
(defun appearance/init ()
  
  ;; Theme Init
  (theme/monokai-init)
  
  ;; Scopes Init
  (scope/beacon-init)
  (scope/winmove-init)
  (scope/highlight-indent-init)
  (scope/better-default-init)
  (scope/anything-init))
