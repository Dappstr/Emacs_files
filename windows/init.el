(global-display-line-numbers-mode)  ;; Add any other settings you want to veri)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e6dc9dfb08b94357a348332644592e59e5292cc877910072ab985680c91edec" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Add the custom theme directory to the load path
(add-to-list 'custom-theme-load-path "C:/Users/laneb/Documents/emacs_files")

;; Load the custom theme
(load-theme 'lensor t)  ; The 't' argument confirms loading without asking
