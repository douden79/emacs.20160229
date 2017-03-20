(defun magit/magit-init ()
  "Magit use-package intialize."
(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull))
  :config (setq magit-save-repository-buffers 'dontask))
)


(defun magit/init ()
  "magit use-package initialize."
  (magit/magit-init)
  )
  
