;;; packages.el --- smex Layer packages File for babel.
(defun smex/init-smex ()
  (use-package smex
    :defer t
    :init
    (global-set-key (kbd "M-x" . smex))))

;; ▶ General
;; ▼ CodeComplete ( Autocomplete, yasnippet )
;; ▼ Appearance
;; ▼ Editor tabs
;; ▼ code folding

;; ▶ Color / Fonts
;; ▶ Code Style
;; ▶ Inspections
;; ▶ File Encoding

