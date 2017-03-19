;; core-emacs.el file.
;;

(require 'core-load-path)
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;;
;; define emacs removes gui elements.
(defun emacs/removes-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars"
  ;; remove menu bar
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  ;; tooltips in echo-area
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

;;
;; define emacs add gui elements.
(defun emacs/add-gui-elements ()
  "Add linum format and cursor color"
  (setq linum-format "%-4d")
  (set-cursor-color "Red"))

;;
;; gui element initialize.
(defun emacs/gui-init ()
  "Init gui element initialize."
  (emacs/add-gui-elements)
  (emacs/removes-gui-elements))

;; emacs initialized define area.
(defun emacs/init ()

  ;; default prefer coding style utf-8
  (prefer-coding-system 'utf-8)
  
  ;; conofigure layer initialize.
;;  (require 'core-configuration-layer)
;;  (configuration-layer/initialize)

  ;; core-load-path init
  (emacs/load-path-init)
  ;; default theme initialize.
  ;; TODO : late load theme.
  ;; load-theme init first.
;;  (load-theme 'monokai t)

  ;; gui element init.
  (emacs/gui-init)
  
  ;; font
  )
(provide 'core-emacs)
