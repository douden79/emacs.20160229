;;+-----------------------~------------------------~---------------------+
;;|                           Emacs's GUI                                |
;;+-----------------------~------------------------~---------------------+

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;;-------------------  Cursor
(set-cursor-color "SkyBlue2")

;;-------------------  menu and scroll
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;-------------------  Line
(global-hl-line-mode +1)
(setq linum-format "%-4d")

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

;;------------------- tabbar ruler
;; buffer groups function
(require 'tabbar)
(tabbar-mode 1)

(setq tabbar-buffer-groups-function nil)

;; dolist button
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
               (set btn (cons (cons "" nil)
                             (cons "" nil))))

;; separator
(setq tabbar-separator '(2.2))

;; tabbar attribute color and ui
(set-face-attribute
 'tabbar-default nil
 :family "Envy Code R VS"
 :background "#34495E"
 :foreground "#EEEEEE"
 :height 0.95
 )
(set-face-attribute
 'tabbar-unselected nil
 :background "#34495E"
 :foreground "#EEEEEE"
 :box nil
)
(set-face-attribute
 'tabbar-modified nil
 :background "#E67E22"
 :foreground "#EEEEEE"
 :box nil
)
(set-face-attribute
 'tabbar-selected nil
 :background "#E74C3C"
 :foreground "#EEEEEE"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)
(set-face-attribute
 'tabbar-separator nil
 :height 1.5)

;; tabbar display buffers-menu-buffer-name-length
(defvar tabbar-displayed-buffers
  '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
  "*Regexps matches buffer names always included tabs.")

;; tabbar displayed buffers
(defvar tabbar-displayed-buffers
  '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
  "*Regexps matches buffer names always included tabs.")

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(setq tabbar-buffer-list-function 'tabbar-buffer-list)

;; Ctrl-Tab, Ctrl-Shift-Tab 
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))

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

;;-------------------  sublimity
(require 'sublimity)
(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)

(setq sublimity-scroll-weight 2
      sublimity-scroll-drift-length 2)
(setq sublimity-attractive-centering-width 110)

(sublimity-mode 1)

(provide 'gui)
;; ui.el ends here!!!!
