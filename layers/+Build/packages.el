;; ▶ Compiler
;; ▶ Debugger
(defun debugger/init-gdb-mi ()
  (use-package gdb-mi
    :ensure t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))
;; ▶ Path Variable

;; Build/Debugging init
(defun build/init ()
  "Build/Debugging init"
  
  ;; debugger
  (debugger/init-gdb-mi)
  )
