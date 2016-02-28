;;+--------------------------------~-----------------------~-------------------------+
;;|                              Emacs's Keybindings                                 |
;;+--------------------------------~-----------------------~-------------------------+

;;------------------- Iedit global edit
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;------------------- move another window cursor.
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;------------------- set multiple cusor
(require 'multiple-cursors)
(global-set-key (kbd "C-l") 'mc/edit-lines)
(global-set-key (kbd "C-;") 'mc/mark-all-words-like-this)

;;------------------- Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

;;------------------- ECB
(global-set-key (kbd "<f10>") 'ecb-activate)

(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f4>") 'highlight-symbol-remove-all)

;;------------------- Magit
(global-set-key (kbd "C--") 'magit-log-all)
(global-set-key (kbd "C-=") 'magit-commit)

;;------------------- Helm Gtags
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-t") 'helm-gtags-pop-stack)
             (local-set-key (kbd "M-]") 'helm-gtags-find-tag)
             (local-set-key (kbd "M-[") 'helm-gtags-find-rtag)
             (local-set-key (kbd "M-.") 'helm-gtags-dwim)
             (local-set-key (kbd "M-,") 'helm-gtags-tags-in-this-function)
             (local-set-key (kbd "C-j") 'helm-gtags-select)
             (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)))

;;------------------- Helm cscope
(add-hook 'helm-cscope-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c .") 'helm-cscope-find-this-symbol)
             (local-set-key (kbd "C-c d") 'helm-cscope-find-global-definition)
             (local-set-key (kbd "C-c c") 'helm-cscope-find-called-function)
             (local-set-key (kbd "C-c p") 'helm-cscope-find-calling-this-function)
             (local-set-key (kbd "C-c s") 'helm-cscope-pop-mark)))

;; binding key for helm-cscope
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode)

(provide 'keybindings)
