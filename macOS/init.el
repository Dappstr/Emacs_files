;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load your theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-hook 'after-init-hook (lambda () (load-theme 'lensor t)))

;; Your existing configurations follow here...
;; ...

(global-display-line-numbers-mode)

;; Tree-sitter configuration using use-package
(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Define custom faces and mappings...
;; ...
(defface tree-sitter-hl-face:operator
  '((t (:foreground "#2da50e" :inherit nil)))
  "Face for operators in tree-sitter syntax highlighting."
  :group 'tree-sitter-hl-faces)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "light sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta"))))
 '(tree-sitter-hl-face:function.call ((t (:foreground "#ffebbb" :inherit nil))))
 '(tree-sitter-hl-face:operator ((t (:foreground "#0aff8d" :inherit nil)))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-<down>" . mc/mark-next-like-this)
         ("C-S-<up>" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Spawn more cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-S-<down>") 'mc/mark-next-like-this)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))

(defun add-cursor-above ()
  (interactive)
  (mc/mark-previous-like-this 1))

(defun add-cursor-below ()
  (interactive)
  (mc/mark-next-like-this 1))

;;(global-set-key (kbd "C-M-S-<up>") 'add-cursor-above)
;;(global-set-key (kbd "C-M-S-<down>") 'add-cursor-below)

(use-package d-mode
  :ensure t)

;;(add-hook 'c-mode-hook #'lsp)
;;(add-hook 'c++-mode-hook #'lsp)
(add-hook 'd-mode #'lsp)  
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-mode nasm-mode eglot org zig-mode ligature rainbow-delimiters flycheck company lsp-mode tree-sitter-langs multiple-cursors)))


;; Eglot
(require 'eglot)
(add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
(add-to-list 'eglot-server-programs '(zig-mode . ("zls")))

;; Enable Company Mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Configure Company Mode to work well with Eglot
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; Instant completion (adjust if needed)

;; Use Company Mode with Eglot
(add-hook 'eglot-managed-mode-hook (lambda () (company-mode)))

;; Automatically start Eglot for C, C++, and Zig files
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)


(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
;; Use actual tab characters
(setq-default indent-tabs-mode t)

;; Set the tab width
;;(setq-default tab-width 4) ;; or any other number you prefer

;; Change indentation to 4 spaces
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4) ;; Set the basic indent to 4 spaces
            (setq tab-width 4)      ;; Set tab width to 4 spaces
            (setq indent-tabs-mode nil) ;; Use spaces instead of tabs
            (c-set-offset 'substatement-open 0))) ;; No extra indent for opening braces

(add-hook 'rust-mode-hook #'lsp-deferred)

(require 'rainbow-delimiters)

;; Enable rainbow-delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(set-frame-font "Fira Code-13" nil t)

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

;; Custom operator highlighting for zig-mode
(defun my-zig-operator-highlighting ()
  "Add custom highlighting for operators in zig-mode."
  (font-lock-add-keywords nil
                          '(("\\(\\+\\|-\\|\\*\\|/\\|%\\|&&\\|||\\|==\\|!=\\|<=\\|>=\\|<\\|>\\|!\\|&\\|\\^\\|~\\|=\\|\\.\\)"
                             0 'font-lock-operator-face))))  ;; Use custom face for operators

;; Add the function to zig-mode-hook
(add-hook 'zig-mode-hook 'my-zig-operator-highlighting)

;; Define the custom face if not already defined
(defface font-lock-operator-face
  '((t (:foreground "green")))  ;; Customize the face with your preferred color
  "Face for operators.")


(add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
