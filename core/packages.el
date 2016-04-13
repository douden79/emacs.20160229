;;+-----------------------~------------------------~---------------------+
;;| babel-packages.el --- Emacs babel : default package selection.       |
;;+----------------------~-------------------------~---------------------+
(require 'cl)
(require 'package)

;;==> package repositories.
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
                         
;;==> set package-user-dir to be relative to babel install path
(setq package-user-dir (expand-file-name "melpa" babel-dir))
(package-initialize)

;;-----------------~-----------------~--------------------------------
;; Package List
;;----------------------~---------------------~-----------------------
(defvar babel-packages
  '(
    ;;-------------- GUI.
    material-theme
    leuven-theme

    highlight-symbol
    ;;-------------- Enhanced.
    hlinum
    linum
    smex
    ;;-------------- Edit.
    smartparens
    vlf
    ;;-------------- IDE
    ecb
    auto-complete
    yasnippet
    function-args
    company
    helm
    ;;-------------- Default.
    tabbar-ruler
    better-defaults
    iedit
    linum
    hlinum
    magit
    multiple-cursors
    tabbar-ruler
    ;;-------------- C/C++
    flymake-cursor
    ;;-------------- Syntax
    flymake
    ;;-------------- Tagging
    helm-cscope
    ggtags
    helm-gtags
    )
  "A list of packages to ensure are installed at launch.")

(defun babel-packages-installed-p ()
  "Check if all packages in `babel-packages' are installed."
  (every #'package-installed-p babel-packages))

(defun babel-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package babel-packages)
    (add-to-list 'babel-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun babel-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'babel-require-package packages))

(define-obsolete-function-alias 'babel-ensure-module-deps 'babel-require-packages)

(defun babel-install-packages ()
  "Install all packages listed in `babel-packages'."
  (unless (babel-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs babel is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (babel-require-packages babel-packages)))

;; run package installation
(babel-install-packages)

(defun babel-list-foreign-packages ()
  "Browse third-party packages not bundled with babel.
Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `babel-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list babel-packages)))

(provide 'packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; packages.el ends here!!!!.
