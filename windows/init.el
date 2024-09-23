;; Enable global line numbers
(global-display-line-numbers-mode)

;; Add MELPA repository for package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Refresh package list if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed (for easier package management)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Install Tree-sitter and Tree-sitter languages packages
(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Enable Tree-sitter for specific major modes excluding Zig
(dolist (hook '(c-mode-hook c++-mode-hook rust-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))

;; Remove explicit Zig grammar setting, relying on tree-sitter-langs instead
;; No need to set (setq treesit-language-source-alist '((zig . ("C:/msys64/mingw64/bin/libtree-sitter-zig.dll"))))

;; Install and configure zig-mode
(use-package zig-mode
  :ensure t)

;; Disable Tree-sitter for zig-mode
(with-eval-after-load 'zig-mode
  (add-hook 'zig-mode-hook (lambda ()
                             (tree-sitter-mode -1)    ;; Disable tree-sitter-mode
                             (tree-sitter-hl-mode -1) ;; Disable tree-sitter highlighting
                             (my-zig-operator-highlighting)))) ;; Apply custom operator highlighting

;; Custom operator highlighting for zig-mode
(defun my-zig-operator-highlighting ()
  "Add custom highlighting for operators in zig-mode."
  (font-lock-add-keywords nil
                          '(("\\(\\+\\|-\\|\\*\\|/\\|%\\|&&\\|||\\|==\\|!=\\|<=\\|>=\\|<\\|>\\|!\\|&\\|\\^\\|~\\|=\\|\\.\\)"
                             0 'font-lock-operator-face))))  ;; Use custom face for operators

;; Define face for operators
(defface font-lock-operator-face
  '((t (:foreground "green"))) ;; Set your desired color for operators
  "Face for highlighting operators.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e6dc9dfb08b94357a348332644592e59e5292cc877910072ab985680c91edec" default))
 '(package-selected-packages
   '(rainbow-delimiters multiple-cursors zig-mode ligature org tree-sitter-langs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org) ;; Org mode

(set-frame-font "Fira Code-10" nil t)

;; Load and configure the ligature package
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/==" "/>" "//" "///" "&&" "||"
      "||=" "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<" "=/="
      ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--"
      "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>"
      "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;; Add the custom theme directory to the load path
(add-to-list 'custom-theme-load-path "C:/Users/laneb/Documents/emacs_files")

;; Load the custom theme
(load-theme 'lensor t)  ; The 't' argument confirms loading without asking

;; Spawn more cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-S-<down>") 'mc/mark-next-like-this)

;; Change indentation to 4 spaces
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4) ;; Set the basic indent to 4 spaces
            (setq tab-width 4)      ;; Set tab width to 4 spaces
            (setq indent-tabs-mode nil) ;; Use spaces instead of tabs
            (c-set-offset 'substatement-open 0))) ;; No extra indent for opening braces

;; Load and configure rainbow-delimiters
(require 'rainbow-delimiters)
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "light sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta")))))

;; Enable rainbow-delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
