;; emacs core-gui initialize.
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is

(global-hl-line-mode +1)

;;(set-face-background 'hl-line "#000094")
(set-face-foreground 'highlight nil)

;;------------------- Linum Scaled display
(require 'linum)
(global-linum-mode t)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
		      (ceiling (* (if (boundp 'text-scale-mode-step)
				      (expt text-scale-mode-step
					    text-scale-mode-amount) 1)
				  (if (car (window-margins))
				      (car (window-margins)) 1)
				  ))))

(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;;------------------- highlight-symbol
(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1")) ;; 使いたい色を設定、repeatしてくれる。

;;------------------- flymake
(require 'flymake)
;; flymake error
(set-face-background 'flymake-errline "#E74C3C")
(set-face-foreground 'flymake-errline "white")

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
;;-------------------  hlinum
(require 'hlinum)
(hlinum-activate)

(provide 'core-gui)
;; ui.el ends here!!!!
