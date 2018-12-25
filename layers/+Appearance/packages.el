;; ▶ THEME : default monokai
(defun theme/monokai-init ()
  "Monokai theme install."
  (use-package monokai-theme
    :ensure t
    :init
    (setq monokai-theme-kit t)
    (load-theme 'monokai t)
    ))

;; ▶ initialize leuven theme.
(defun theme/leuven-init ()
  "Leuven theme install."
  (use-package leuven-theme
    :ensure t
    :init
    (load-theme 'leuven t))
  )

;; ▶ initialize draculra theme.
(defun theme/dracula-init ()
  "Draculra theme install."
  (use-package draculra-theme
    :ensure t
    :init
    (load-theme 'dracula t))
  )

;; ▶ initialize spacemacs theme.
(defun theme/spacemacs-init ()
  "Spacemacs theme install."
  (use-package spacemacs-theme
    :ensure t
    :init
    (load-theme 'spacemacs-light t))
  )

;;  ▶ initialize monokai-alt-theme
(defun theme/monokai-alt-init ()
  "monokai alt theme install."
  (use-package monokai-alt-theme
    :ensure t
    :init
    (load-theme 'monokai-alt t)))

;;  ▶ initialize atom-alt-theme
(defun theme/atom-one-dark-init ()
  "monokai alt theme install."
  (use-package atom-one-dark-theme
    :ensure t
    :init
    (load-theme 'atom-one-dark t)))

;;  ▶ initialize atom-alt-theme
(defun theme/material-init ()
  "material theme install."
  (use-package material-theme
    :ensure t
    :init
    (load-theme 'material t)))

;; ▶ initialize hemisu-theme
(defun theme/hemisu-init ()
  "hemisu theme install."
  (use-package hemisu-theme
    :ensure t
    :init
    (load-theme 'hemisu-light t)))

;; ▶ Menu/ToolBar
;; ▶ System Settings

;; ▼ telephone-line
(defun scope/telephone-init ()
 "telephone line init"
 (use-package telephone-line
  :ensure t
  :init 
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
   telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
   telephone-line-primary-right-separator 'telephone-line-cubed-right
   telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 20
   telephone-line-evil-use-short-tag t)
  (telephone-line-mode t)
 )
 )

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
    (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
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

;; ▼ highlight-symbol
(defun scope/highlight-symbol ()
  "highlight-symbol init"
  (use-package highlight-symbol
    :ensure t
    :bind (([f3] . highlight-symbol-at-point)
           ([f4] . highlight-symbol-remove-all)))
  )

;; ▼ ORG-MODE
(defun scope/org-bullets ()
  "org bullets init"
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    :init
    (setq org-todo-keywords
          '((sequence
             "TODO(t)"  ; next action
             "TOBLOG(b)"  ; next action
             "STARTED(s)"
             "WAITING(w@/!)"
             "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
            (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE(x)")
            (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
            (sequence "TOBUY" "TOSHRINK" "TOCUT"  "TOSEW" "|" "DONE(x)")
            (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))
    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "green" :weight bold))
            ("DONE" . (:foreground "cyan" :weight bold))
            ("WAITING" . (:foreground "red" :weight bold))
            ("SOMEDAY" . (:foreground "gray" :weight bold))))
    (setq org-log-done 'time)
    )
  )
;; ▼ powerline evil
(defun scope/powerline ()
  "powerline evil"
  (use-package powerline-evil
    :ensure t
    :init (evil-mode 1)
    (powerline-default-theme t)
    )
  )

;; ▼ emacs basic appearance
(defun scope/emacs-basic ()
  "emacs basic appearance"
  ;; ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; left/right split window
  (setq ediff-split-window-function 'split-window-horizontally)
  )

    ;; Appearance Init
(defun appearance/init ()
  
  ;; Theme Init
  ;;(theme/monokai-init)
  ;;(theme/leuven-init)
  ;;(theme/spacemacs-init)
  (theme/monokai-alt-init)
  ;;(theme/dracula-init)
  ;;(theme/atom-one-dark-init)
  ;;(theme/material-init)
  ;;(theme/hemisu-init)
  
  ;; Scopes Init
  (scope/beacon-init)
  (scope/winmove-init)
  ;;(scope/highlight-indent-init)
  (scope/better-default-init)
  (scope/anything-init)
  (scope/telephone-init)

  ;; scope org init
  (scope/org-bullets)
  ;; emacs basic
  (scope/emacs-basic)
  )
