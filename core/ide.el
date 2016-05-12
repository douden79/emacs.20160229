;;-------------------- CEDET
(defun babel-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hook 'babel-semantic-hook)
(setq global-semantic-idle-summary-mode t)
(setq global-semanticdb-minor-mode t)
(setq global-semantic-idle-summary-mode t)
(setq global-semantic-highlight-func-mode t)
(setq global-semantic-decoration-mode t)
(setq global-semantic-mru-bookmark-mode t)
(setq global-semantic-stickyfunc-mode t)

(provide 'ide)
