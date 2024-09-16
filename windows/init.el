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
  :ensure t
  :hook (prog-mode . global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Enable Tree-sitter for specific major modes
(dolist (hook '(c-mode-hook c++-mode-hook zig-mode-hook rust-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))  ;; Enable highlighting

;; Enable Tree-sitter for Zig mode
;; NOT CURRENTLY WORKING
(add-hook 'zig-mode-hook #'tree-sitter-mode)
(add-hook 'zig-mode-hook #'tree-sitter-hl-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e6dc9dfb08b94357a348332644592e59e5292cc877910072ab985680c91edec" default))
 '(package-selected-packages '(ligature org tree-sitter-langs)))
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
  :ensure t  ;; Ensures that the package is installed
  :config
  ;; Enable specific ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))

  ;; Enable ligatures in all text and programming modes
  (global-ligature-mode t))

;; Add the custom theme directory to the load path
(add-to-list 'custom-theme-load-path "C:/Users/laneb/Documents/emacs_files")

;; Load the custom theme
(load-theme 'lensor t)  ; The 't' argument c
