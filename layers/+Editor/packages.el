;;; packages.el --- smex Layer packages File for babel.
(defun better/smex ()
  "smex use-package initialize."
  (use-package smex
    :ensure t
    :bind
    ("M-x" . smex)))

;; ▶ General
;; ▼ CodeComplete ( Autocomplete, yasnippet )
;; ▼ Appearance
;; ▼ Editor tabs
;; ▼ code folding

;; ▶ Color / Fonts
;; ▶ Code Style
;; ▶ Inspections
;; ▶ File Encoding

;; Editor init
(defun editor/init ()
  "Editor envirment init"
  (better/smex)
  )
