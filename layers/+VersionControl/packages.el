;; ▶ Magit
(defun magit/magit-init ()
  "Magit use-package intialize."
(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c l" . magit-log-all)
	 ("C-x c" . magit-commit)
	 ("C-x p" . magit-pull))
  :config (setq magit-save-repository-buffers 'dontask)))

;; ▶ Version Control package init.
(defun versioncontrol/init ()
  "Version Control package init."
  (magit/magit-init))
