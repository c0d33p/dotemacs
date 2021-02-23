;; Straight
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

;; Leaf
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(straight-use-package 'leaf-tree)
(straight-use-package 'leaf-convert)
(straight-use-package 'diminish)

(leaf leaf
  :require t
  :init
  (leaf-keywords-init))

;; GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)

;; GUI - Theme
(leaf color-theme-sanityinc-tomorrow
  :straight t)

;; Font
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-frame-font "firacode 10" nil t)

;; Font - ligatury
(straight-use-package
  '(ligature :type git :host github :repo "mickeynp/ligature.el"))

;; Enable the www ligature in every possible major mode

(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes                                                           
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)

;; Font errors patch
(defun my/ansi-colorize-buffer ()
(let ((buffer-read-only nil))
(ansi-color-apply-on-region (point-min) (point-max))))

(leaf ansi-color
  :straight t
  :config
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
  )

;; QoL
(setq ring-bell-function 'ignore)
(electric-pair-mode 1)
(setq-default tab-width 4)
(show-paren-mode 1)

;; QoL - yes/no == y/n
(fset 'yes-or-no-p 'y-or-n-p) 

;; Backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
   backup-by-copying t   
   version-control t     
   delete-old-versions t 
   kept-new-versions 20  
   kept-old-versions 5   
   )



;; Key-Chord
(leaf key-chord
  :straight t
  :init 
  :config (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.4)
  (setq key-chord-one-key-delay 0.5))

;; PROJECTILE
(leaf projectile
  :straight t
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; HELM
(leaf helm
  :straight t
  :init
  (helm-mode 1)
  (progn (setq helm-buffers-fuzzy-matching t))
  :bind
  ("M-x"     . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-r" . helm-recentf)
  ("C-c h"   . helm-command-prefix)
  ("C-x b"   . helm-buffer-list)
  ("C-c b"   . helm-bookmarks)
  ("C-x a"   . helm-mini)
  ("C-c g"   . helm-grep-do-git-grep))

(leaf helm-descbinds
  :straight t
  :bind
  ("C-h b"   . helm-descbinds))

(leaf helm-swoop
  :straight t
  :load-path "site-lisp/helm-swoop"
  :chord
  ("js"     . helm-swoop)
  ("jp"     . helm-swoop-back-to-last-point)
  :init
  (define-key isearch-mode-map (kbd "M-m") 'helm-occur-from-isearch)
  (setq helm-swoop-use-fuzzy-match t)
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color nil)
  (setq helm-swoop-move-to-line-cycle t))

;; Rainbow Delimiters
(leaf rainbow-delimiters
  :straight t
  :init
  :hook (prog-mode-hook)
  
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "gold") 
  (set-face-foreground 'rainbow-delimiters-depth-2-face "red") 
  (set-face-foreground 'rainbow-delimiters-depth-3-face "cyan") 
  (set-face-foreground 'rainbow-delimiters-depth-4-face "spring green") 
  (set-face-foreground 'rainbow-delimiters-depth-5-face "pink") 
  (set-face-foreground 'rainbow-delimiters-depth-6-face "magenta") 
  (set-face-foreground 'rainbow-delimiters-depth-7-face "goldenrod") 
  (set-face-foreground 'rainbow-delimiters-depth-8-face "IndianRed1") 
  (set-face-foreground 'rainbow-delimiters-depth-9-face "ivory1") 
  (set-face-bold 'rainbow-delimiters-depth-1-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-2-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-3-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-4-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-5-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-6-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-7-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-8-face "t") 
  (set-face-bold 'rainbow-delimiters-depth-9-face "t"))


;; AVY

(leaf avy
  :straight t
  :chord
  ("jc"  .  avy-goto-char)
  ("jw"  .  avy-goto-word-1)
  ("jl"  .  avy-goto-line))

;; WHICH-KEY
(leaf which-key
  :straight t
  :init
  (which-key-mode))

;; QUICKRUN
(leaf quickrun
  :straight t
  :bind
  ("C-c C-r"  .  quickrun))

;; Languages

(straight-use-package 'nix-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#002451" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#ffffff"))
 '(beacon-color "#ff9da4")
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(fci-rule-color "#003f8e")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
