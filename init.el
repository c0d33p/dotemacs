;; Straight i leaf
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications '(check-on-save-find-when-checking))
(require 'straight-x)

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(straight-use-package 'diminish)

(leaf leaf
      :require t
      :init
      (leaf-keywords-init))

;; Wyłączenie GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)

;; yes/no == y/n
(fset 'yes-or-no-p 'y-or-n-p) 
