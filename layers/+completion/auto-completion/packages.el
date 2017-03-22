;;; packages.el --- Auto-completion Layer packages File for babel.

(setq auto-completion-packages
      '(
        auto-complete
        ac-ispell
        company
        company-quickhelp
        company-statistics
        helm-company
        helm-c-yasnippet
        hippie-exp
        yasnippet
        auto-yasnippet
        smartparens
        ))

;; TODO replace by company-ispell which comes with company
;; to be moved to spell-checking layer as well
(defun auto-completion/init-ac-ispell ()
  (use-package ac-ispell
    :ensure t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (with-eval-after-load 'auto-complete
        (ac-ispell-setup))
      ;; (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)
      )))

(defun auto-completion/init-auto-complete ()
  (use-package auto-complete
    :ensure t
    :init
    (setq ac-auto-start 0
          ac-delay 0.2
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ;; use 'complete when auto-complete is disabled
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (setq-default ac-sources '(ac-source-abbrev
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers))
      (add-to-list 'completion-styles 'initials t)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous))))

(defun auto-completion/init-company ()
  (use-package company
    :ensure t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil)

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
    :config
    ;; key bindings
    "Complete common prefix or cycle backward."
    (interactive)
    (company-complete-common-or-cycle -1)

    (let ((map company-active-map))
      (define-key map (kbd "C-/")   'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d")   'company-show-doc-buffer))))

(defun auto-completion/init-company-statistics ()
  (use-package company-statistics
    :if auto-completion-enable-sort-by-usage
    :ensure t
    :init
    (progn
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(defun auto-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :ensure t
    :commands company-quickhelp-manual-begin
    :init
    (with-eval-after-load 'company
      (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
      (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
      (unless (eq auto-completion-enable-help-tooltip 'manual)
	(company-quickhelp-mode)))))

(defun auto-completion/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :ensure t
    :init
    (progn
      (setq helm-c-yas-space-match-any-greedy t))))

(defun auto-completion/init-helm-company ()
  (use-package helm-company
    :ensure t
    :init
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-/") 'helm-company))))

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))))

(defun auto-completion/init-yasnippet ()
  (use-package yasnippet
    :ensure t
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t
            helm-yas-display-key-on-candidate t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
      ;; configure snippet directories
)))


(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :ensure t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or auto-completion-private-snippets-directory
                (concat configuration-layer-private-directory "snippets/")))
      )))

(defun auto-complete/init ()
  "autocomplete with package."
  (auto-completion/init-ac-ispell))
;;  (auto-completion/init-auto-complete)
;;  (auto-completion/init-company)
;;  (auto-completion/init-company-statistics)
;;  (auto-completion/init-company-quickhelp)
;;  (auto-completion/init-helm-c-yasnippet)
;;  (auto-completion/init-helm-company)
;;  (auto-completion/init-hippie-exp)
;;  (auto-completion/init-yasnippet)
;;  (auto-completion/init-auto-yasnippet))
