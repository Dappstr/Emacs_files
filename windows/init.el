;; -*- lexical-binding: t; -*-

;; PACKAGES REQUIRED: auctex, company, eglot, haskell-mode, latex-preview-pane, ligatures, magit, multiple-cursors, org, python-mode, rainbow-delimiters, rust-mode, tree-sitter-langs, zig-mode, compat, dash, eldoc, jsonrpc, magit-selection, reformatter, transient, tree-sitter, tsc, with-editor

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

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e6dc9dfb08b94357a348332644592e59e5292cc877910072ab985680c91edec" default))
 '(package-selected-packages
   '(auctex latex-preview-pane python-mode haskell-mode rust-mode magit company eglot rainbow-delimiters multiple-cursors zig-mode ligature org tree-sitter-langs)))
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
 '(rainbow-delimiters-depth-9-face ((t (:foreground "magenta")))))

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


;; Enable rainbow-delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; For getting the keys to install eglot
(setq epg-gpg-program "C:/Program Files (x86)/GnuPG/bin/gpg.exe")

;; Eglot
(require 'eglot)
(add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
(add-to-list 'eglot-server-programs '(zig-mode . ("zls")))

;; Automatically start Eglot for C and C++ files
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

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

;; CONFIG FOR USING GIT'S BASH AS THE DEFAULT SHELL

;; Set the shell executable path
(setq explicit-shell-file-name "C:/Progra~1/Git/bin/bash.exe")
(setq shell-file-name explicit-shell-file-name)

;; Specify arguments for bash
(setq explicit-bash.exe-args '("--login" "-i"))

(add-to-list 'exec-path "C:/Program Files/Git/bin")
(setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))


;; Display startup info in new window
;;(defun display-startup-info ()
;;  "Open a buffer displaying startup time and number of loaded packages."
;;  (let ((load-time (float-time (time-subtract after-init-time before-init-time)))
;;        (package-count (length package-activated-list)))
;;    (with-current-buffer (get-buffer-create "*Startup Info*")
;;      (erase-buffer)
;;      (insert (format "Emacs started in %.2f seconds\n" load-time))
;;      (insert (format "Loaded %d packages\n" package-count))
;;      (goto-char (point-min))
;;      (read-only-mode 1))
;;    ;; Display the *Startup Info* buffer
;;    (display-buffer "*Startup Info*")))
;;
;;;; Use `emacs-startup-hook` to run this function after startup
;;(add-hook 'emacs-startup-hook 'display-startup-info)


;; CUSTOM SPLASH SCREEN
(defun my-splash-screen ()
  "Display custom ASCII art with Emacs load stats on startup."
  (switch-to-buffer "*splash*")
  (erase-buffer)
  ;; Insert ASCII Art
  (insert "
  , .
 o   \\\\
      \\\\
       \\\\
        \\\\
         \\\\
         / \\
        // \\\\
       //   \\\\
      //     \\\\
     //       \\\\_,	

")
  ;; Add the highlighted text
  (insert (propertize "    Remember Lens_r" 'face '(:foreground "yellow")))

  ;; Display package and load time info
  (let ((package-count (length package-activated-list))
        (load-time (emacs-init-time)))
    (insert (format "\n\nLoaded %d packages in %s\n" package-count load-time)))
  ;; Make buffer read-only and display it
  (setq buffer-read-only t)
  (goto-char (point-min)))

;; Show splash screen only once
(add-hook 'emacs-startup-hook 'my-splash-screen)
(setq inhibit-startup-message t)  ;; Disable default splash screen

;; Remove tool bar and menu bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Disable audible bell
(setq ring-bell-function 'ignore)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))

;; Set indentation for text files
(add-hook 'text-mode-hook
          (lambda ()
            (setq-local tab-width 4)        ;; Set tab width to 4 spaces
            (setq-local indent-tabs-mode t))) ;; Use tabs for indentation


;; Config for building and running (C/C++) projects within GNU Emacs
;; MUST USE CMAKE WITH "add_executable(...)"

(defun find-executable-name (project-root)
  "Find the executable name from the CMakeLists.txt file in the project root."
  (let ((cmake-file (concat project-root "CMakeLists.txt")))
    (with-temp-buffer
      (insert-file-contents cmake-file)
      ;; Regex to find the executable name in add_executable
      (if (re-search-forward "add_executable\\s-*[(]\\s-*\\([a-zA-Z0-9_]+\\)" nil t)
          (match-string 1)
        (error "No executable defined in CMakeLists.txt")))))

(defun compile-and-run ()
  "Compile and run a CMake project within Emacs with optional arguments."
  (interactive)
  (let* ((project-root (locate-dominating-file default-directory "CMakeLists.txt"))
         (build-dir (concat project-root "build/"))
         (executable-name (find-executable-name project-root)) ;; Use dynamic executable name
         (executable-path (concat build-dir executable-name (if (eq system-type 'windows-nt) ".exe" "")))
         (output-buffer "*run-output*")
         (args (read-string "Arguments for the executable (leave blank for none): ")))
    (unless project-root
      (error "No CMakeLists.txt found in the project hierarchy"))
    ;; Ensure the build directory exists
    (unless (file-directory-p build-dir)
      (make-directory build-dir t))
    ;; Set the build directory as default-directory for compile
    (let ((default-directory build-dir))
      ;; Compile the project
      (compile "cmake --build .")
      ;; Wait for the compilation buffer to be created
      (with-current-buffer "*compilation*"
        (let ((output-buffer output-buffer)) ;; Capture `output-buffer` in the sentinel
          ;; Set up a process sentinel to handle the build result
          (set-process-sentinel
           (get-buffer-process (current-buffer))
           (lambda (_proc _msg)
             (with-current-buffer "*compilation*"
               (goto-char (point-max))
               (cond
                ;; Handle "no work to do"
                ((re-search-backward "no work to do" nil t)
                 (message "Nothing to build; running the executable anyway.")
                 (with-current-buffer (get-buffer-create output-buffer)
                   (erase-buffer))
                 (apply 'start-process "run-executable" output-buffer executable-path (split-string-and-unquote args))
                 (switch-to-buffer output-buffer))
                ;; Handle "Build finished"
                ((re-search-backward "Build finished\\." nil t)
                 (message "Build finished; running the executable.")
                 (with-current-buffer (get-buffer-create output-buffer)
                   (erase-buffer))
                 (apply 'start-process "run-executable" output-buffer executable-path (split-string-and-unquote args))
                 (switch-to-buffer output-buffer))
                ;; Handle errors
                (t
                 (message "Compilation failed.")))))))))))


;; SWITCH TO TEXT-MODE WHEN IN THE SCRATCH BUFFER
(defun my-scratch-mode ()
  "Set the `*scratch*` buffer to `text-mode`."
  (when (string= (buffer-name) "*scratch*")
    (text-mode)))

(add-hook 'after-change-major-mode-hook 'my-scratch-mode)

(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.lat\\'" . LaTeX-mode))
