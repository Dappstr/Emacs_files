
(global-display-line-numbers-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'lensor t)

(require 'treesit)

;; List of languages you want to enable with Tree-sitter
(dolist (lang '(c c++ zig rust))
  ;; Enable tree-sitter-based major modes
  (add-to-list 'major-mode-remap-alist
               `(,(intern (concat (symbol-name lang) "-mode"))
                 . ,(intern (concat (symbol-name lang) "-ts-mode")))))

;; Correctly specify grammar sources with compilation instructions
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" nil nil)
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" nil nil)
        (zig "https://github.com/GrayJack/tree-sitter-zig" nil nil)
        (rust "https://github.com/tree-sitter/tree-sitter-rust" nil nil)))

;; Install missing Tree-sitter grammars automatically
(defun my-install-missing-grammars ()
  "Install missing Tree-sitter grammars."
  (interactive)
  (dolist (lang '(c cpp zig rust))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; Call the function to install the grammars
(my-install-missing-grammars)

;; Add MELPA to the list of package archives
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
changed ownership of './zig-out/share/ghostty/themes/Nocturnal Winter' from root to dappster
;; Initialize the package system
(package-initialize)
changed ownership of './zig-out/share/ghostty/themes/NvimLight' from root to dappster
;; Ensure package lists are updated
(unless package-archive-contents
  (package-refresh-contents))
changed ownership of './zig-out/share/ghostty/themes/OceanicMaterial' from root to dappster
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
[dappster@arch ~]$ cat .emacs.d/init.el
;; Enable line numbers globally
(global-display-line-numbers-mode)

;; Load custom theme from your themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'lensor t)

;; Initialize and configure package system with MELPA and GNU ELPA
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Refresh package contents if not already available
(unless package-archive-contents
  (package-refresh-contents))

;; Install selected packages
(custom-set-variables
 '(package-selected-packages '(eglot)))

;; Load and configure Eglot for language server support
(require 'eglot)
(add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd")))
(add-to-list 'eglot-server-programs '(c-ts-mode . ("clangd")))

;; Configure Tree-sitter and enable its major modes for supported languages
(require 'treesit)
(dolist (lang '(c c++ zig rust))
  (add-to-list 'major-mode-remap-alist
               `(,(intern (concat (symbol-name lang) "-mode"))
                 . ,(intern (concat (symbol-name lang) "-ts-mode")))))

;; Define and install missing Tree-sitter grammars automatically
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" nil nil)
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" nil nil)
        (zig "https://github.com/GrayJack/tree-sitter-zig" nil nil)
        (rust "https://github.com/tree-sitter/tree-sitter-rust" nil nil)))

(defun my-install-missing-grammars ()
  "Install missing Tree-sitter grammars."
  (interactive)
  (dolist (lang '(c cpp zig rust))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(my-install-missing-grammars)

;; Define a custom face for operators
(defface my-operator-face
  '((t (:foreground "#0aff8d" :inherit nil)))
  "Custom face for operators like ->, ::, <<, etc.")

;; Function to apply custom highlighting for operators
(defun my-apply-custom-operator-highlighting ()
  "Apply custom highlighting for a wide range of operators."
  (font-lock-add-keywords nil
                          '(("->\\|::\\|<<\\|>>\\|==\\|!=\\|<=\\|>=\\|=\\|+\\|-\\|*\\|/\\|%\\|&\\||\\|^\\|~\\|\\.\\|<\\|>"
                             . 'my-operator-face))
                          'append))

;; Add custom operator highlighting to Tree-sitter mode hooks
(add-hook 'c++-ts-mode-hook 'my-apply-custom-operator-highlighting)
(add-hook 'c-ts-mode-hook 'my-apply-custom-operator-highlighting)

(custom-set-faces
 '(tree-sitter-hl-face:function.call ((t (:foreground "#ffebbb" :inherit nil))))
 '(tree-sitter-hl-face:operator ((t (:foreground "#0aff8d" :inherit nil)))))
