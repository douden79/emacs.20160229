;;+--------------------------------~-----------------------~-------------------------+
;;|                                 Emacs's error env.                               |
;;+--------------------------------~-----------------------~-------------------------+

;;------------------- error
(setq ecb-layout-name "emacs")
(setq ecb-examples-bufferinfo-buffer-name nil)
; stack-trace-on-error
(setq stack-trace-on-error t)
(setq ecb-version-check nil)
(setq ecb-compile-window-height 12)

;;------------------- Max lisp eval depth
(setq max-lisp-eval-depth 20000)
(provide 'core-error)
