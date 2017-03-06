;; core-emacs.el file.
;;

(require 'core-configuration-layer)

(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(defun emacs/init ()
  (emacs/configuration-layer-init)
  )
(provide 'core-emacs)
