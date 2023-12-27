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

;;(with-eval-after-load 'tree-sitter
;;  (custom-set-faces
;;   '(tree-sitter-hl-face:operator ((t (:foreground "#0aff8d" :inherit nil))))))
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(package-selected-packages '(multiple-cursors tree-sitter-langs tree-sitter)))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(tree-sitter-hl-face:operator ((t (:foreground "#0aff8d" :inherit nil)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tree-sitter-hl-face:function.call ((t (:foreground "#ffebbb" :inherit nil))))
 '(tree-sitter-hl-face:operator ((t (:foreground "#0aff8d" :inherit nil)))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(defun add-cursor-above ()
  (interactive)
  (mc/mark-previous-like-this 1))

(defun add-cursor-below ()
  (interactive)
  (mc/mark-next-like-this 1))

(global-set-key (kbd "C-M-S-<up>") 'add-cursor-above)
(global-set-key (kbd "C-M-S-<down>") 'add-cursor-below)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"))  ; Set your desired prefix key here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company lsp-mode tree-sitter-langs multiple-cursors)))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)  ; Adjust as per your preference
  :hook (after-init . global-company-mode))
