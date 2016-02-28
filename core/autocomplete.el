;;+--------------------------------~-----------------------~-------------------------+
;;|                                  Emacs's AutoComplete                            |
;;+--------------------------------~-----------------------~-------------------------+

;;------------------- Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;------------------- auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
; auto complete c header
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.8",
               '"/usr/include/c++/4.8/backward",
               '"/usr/local/include",
               '"/usr/include"
               ))
;; ac header default enable
(add-hook 'c++-mode 'my:ac-c-header-init)
(add-hook 'c-mode 'my:ac-c-header-init)

(provide 'autocomplete)
