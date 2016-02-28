;;-------------------- CEDET
(defun babel-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hook 'babel-semantic-hook)

(provide 'ide)
