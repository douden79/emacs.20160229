(defun theme/monokai-init ()
  "Monokai theme install."
  (use-package monokai-theme
    :ensure t
    :init
    (setq monokai-theme-kit t)
    (load-theme 'monokai t)
    ))

(defun appearance/init ()
  (theme/monokai-init))

;; ▶ Menu/ToolBar
;; ▶ System Settings
;; ▶ Scopes
