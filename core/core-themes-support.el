;; core themes support.el

;;
;; get theme package in layer directory.
;; TODO : theme

(defun emacs/load-theme ()
  "default load theme"
  ;; TODO : late define load-theme
  (load-theme 'monokai t))

(provide core-themes-support)
