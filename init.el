;;; Init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs;;; gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
;;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;global settings
(setq inhibit-splash-screen t)
(global-linum-mode 0)
(show-paren-mode t)
(set-face-attribute 'show-paren-match nil
                    :background "yellow")
(setq backup-inhibited t)
(setq create-lockfiles nil)
(size-indication-mode t)
(setq scroll-step 1)
(menu-bar-mode -1)
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key global-map (kbd "C-t") 'other-window)
(electric-pair-mode t)
(column-number-mode 1)
(global-auto-revert-mode 1)
(setq select-enable-clipboard t)
(which-function-mode)
;; git clone https://github.com/grammati/windsize.git ~/.emacs.d/windsize
(add-to-list 'load-path (expand-file-name "~/.emacs.d/windsize"))
(require 'windsize)
(windsize-default-keybindings)
(setq windsize-cols 16)
(setq windsize-rows 8)


;;usually, settings about indentation are written for each language.
;;indent settings
(setq-default indent-tabs-mode nil) ;; use space, not tabs for indent
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 2 60 2))

;; whitespace-mode
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(face           ;display with face
                         tabs
                         ;;spaces
                         trailing
                         empty
                         ;;                         space-mark     ;mappings
                         tab-mark))
;; whitespace
(set-face-foreground 'whitespace-space "color-51")
(set-face-background 'whitespace-space nil)
;; tab
(set-face-foreground 'whitespace-tab "color-51")
(set-face-background 'whitespace-tab nil)

(setq whitespace-display-mappings
      '(
        (space-mark ?\x3000 [?\□])
        (space-mark 32 [183] [46])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
        ))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package spaceline
  :ensure t
  )

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme)
  (spaceline-toggle-which-function-on)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-projectile-root-on)
  )

(use-package dracula-theme
  :ensure t
  )


(use-package hydra
  :ensure t)

;;major-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package nasm-mode
  :ensure t
  :mode "\\.asm\\'")

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        ;;web-mode-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-current-element-highlight t
        )
  )

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :bind((:map markdown-mode-map)
        ("C-M-y" . livedown-preview)))


(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

;; need to run the below command
;; git clone https://github.com/shime/emacs-livedown.git ~/.emacs.d/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

(use-package lsp-mode
  :ensure t
  :hook
  ((c-mode python-mode rust-mode typescript-mode go-mode) . lsp)
  :commands (lsp)
  :bind
  ("C-c j" . lsp-find-definition)
  :config
  (setq lsp-prefer-flymake nil)
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq-default lsp-pyls-plugins-pydocstyle-enabled t)
  (setq-default lsp-pyls-plugins-pylint-enabled nil)
  )

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; sentence wrap of lsp-ui-doc doesn't work correctly when we split views.
(use-package lsp-ui
  :ensure t
  :hook
  ((lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t)
  )

;; minor-mode
(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-c y" . helm-show-kill-ring)
         ("C-c o" . helm-occur))
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'other)
  )

(use-package helm-file-preview
  :ensure t
  :config
  (helm-file-preview-mode 1)
  )


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (set-face-foreground 'flycheck-error "#FF0461")
  )

(use-package flyspell
  :ensure t
  :hook
  ((python-mode go-mode yaml-mode rust-mode c-mode emacs-lisp-mode dockerfile-mode)
   . flyspell-prog-mode)
  )

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind
  ((:map company-active-map)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))

(use-package yasnippet
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (
         ("C-c m c" . mc/edit-lines)
         ("C-c m k" . mc/mark-all-like-this)
         )
  )

(defun toggle-term-mode()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (add-to-list 'term-unbind-key-list "C-t" "C-c")
  :bind
  ("C-c C-j" . toggle-term-mode)
  )

(define-key global-map (kbd "C-c s") 'multi-term)

(use-package realgud-lldb
  :ensure t
  :defer t
  )
(load "realgud")

(use-package projectile
  :ensure t
  )

(use-package helm-projectile
  :ensure t
  )

(bind-key
 "M-/"
   (defhydra hydra-helm-menu (:exit t :hint nil)
    "
_f_: find file  _d_: find directory  _r_: ripgrep _q_: exit
"
    ("f"
     (if (projectile-project-p)
         (helm-projectile-find-file)
       (helm-find-files-1 default-directory))
     )
    ("d"
     (if (projectile-project-p)
         (helm-projectile-find-dir)
       (helm-find-files-1 default-directory))
     )
    ("r"
     (if (projectile-project-p)
         (helm-projectile-rg)
       (helm-rg ""))
     )
    ("q" keyboard-quit "quit" :color blue)
    )
   )

(use-package magit
  :ensure t
  )

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  )

(use-package dap-mode
  :ensure t
  :commands
  (dap-hydra)
  :hook
  (
   ((c-mode python-mode rust-mode go-mode) . dap-mode)
   ((c-mode python-mode rust-mode go-mode) . dap-ui-mode)
   )
  :bind
  ("C-]" . dap-hydra)
  :init
  (require 'dap-python)
  ;;(require 'dap-lldb)
  )

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;;org-mode settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-startup-truncated nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-checker-error-threshold 800)
 '(org-agenda-files (quote ("~/.notes.org")))
 '(package-selected-packages
   (quote
    (spaceline spaceline-config yaml-mode flycheck-rust epl f flycheck git-commit git-gutter go-mode helm helm-core helm-file-preview helm-lsp helm-projectile ht hydra load-relative loc-changes lsp-mode lsp-ui lv magit markdown-mode multi-term multiple-cursors nasm-mode pkg-info popup projectile realgud realgud-lldb rust-mode s sphinx-doc spinner test-simple transient tree-mode typescript-mode use-package web-mode with-editor yasnippet helm-rg org-preview-html-mode tide))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
