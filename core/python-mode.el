;;+-------------------------~-----------------------~--------------------------------+
;;|                           Emacs's Python Env                                     |
;;+-------------------------~-----------------------~--------------------------------+

;;------------------------ python mode tab-stop
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)))

(provide 'python-mode)
