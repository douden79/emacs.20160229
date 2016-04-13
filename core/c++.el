;;+-------------------------~-----------------------~--------------------------------+
;;|                                Emacs's C++ ENV                                   |
;;+-------------------------~-----------------------~--------------------------------+

;;------------------- flymake-google-cppint-load
;; first. install python-pip
;; second. sudo pip search cpplint
;; third. sudo pip install cpplint
;;-------------------
;;(defun my:flymake-google-init ()
;;  (require 'flymake-google-cpplint)
;;  (custom-set-variables
;;   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
;;  (flymake-google-cpplint-load)
;;  )
;;
;;(add-hook 'c-mode-hook 'my:flymake-google-init)
;;(add-hook 'c++-mode-hook 'my:flymake-google-init)
;;
;;;;------------------- flymake
;;(defun flymake-c-init ()
;;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;         (local-file  (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;    (list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
;;
;;(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
;;
;;(add-hook 'c-mode-hook
;;          '(lambda ()
;;             (flymake-mode t)))
;;
;;------------------- start google-c-style with emacs
;;(require 'google-c-style)
;;(add-hook 'c-mode-common-hook 'google-set-c-style)
;;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;------------------- Default c/c++ indent.
(defun nhk-c-mode ()
  "C mode with adjusted defaults for my use."
  (interactive)
  (c-mode)
  (setq tab-width 4
  c-basic-offset 4
  indent-tabs-mode nil))

(defun linux-c-mode ()
"C mode with adjusted defaults for use with the Linux kernel."
(interactive)
(c-mode)
(c-set-style "K&R")
(setq tab-width 8)
(setq indent-tabs-mode t)
(setq c-basic-offset 8))

(add-to-list 'auto-mode-alist '("\.c$" . linux-c-mode))
;;-------------------- my Linux kernel path
(setq auto-mode-alist
      (cons '("~/linux-4.4.1/.*\\.[ch]$" . linux-c-mode)
            auto-mode-alist))

(provide 'c++)
