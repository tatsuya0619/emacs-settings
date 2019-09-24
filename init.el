;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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
(global-linum-mode t)
(show-paren-mode t)
(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq scroll-step 1)
(menu-bar-mode -1)
(define-key key-translation-map [?\C-h] [?\C-?])
(electric-pair-mode t)
(column-number-mode 1)
(global-auto-revert-mode 1)


;;usually, settings about indentation are written for each language.
;;indent settings
(setq-default indent-tabs-mode nil) ;; use space, not tabs for indent
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 2 60 2))

;; whitespace-mode
(global-whitespace-mode 1)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
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


;;major-mode
;;(add-to-list 'load-path (expand-file-name "${HOME}/.cargo/bin/rls"))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

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

;;(use-package tide
;;  :ensure t
;;  :hook ((typescript-mode . tide-setup)
;;         (typescript-mode . tide-hl-identifier-mode)
;;         (before-save . tide-format-before-save)))

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
        ;;web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  )

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :bind((:map markdown-mode-map)
	("C-M-y" . livedown-preview)))

;; need to run the below command
;; git clone https://github.com/shime/emacs-livedown.git ~/.emacs.d/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (typescript-mode . lsp)
         (go-mode . lsp))
  :commands (lsp)
  :bind
  ("C-c j" . lsp-find-definition)
  :config
  (setq lsp-prefer-flymake nil)
  ;(setq lsp-auto-guess-root t)
  )


;;If I didn't install company-lsp,
;;completion occurs bug.
;;I dont know why.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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
  (helm-mode 1))


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  ;for python3 syntax
  (setq flycheck-python-pycompile-executable "python3"
	flycheck-python-pylint-executable "python3"
	flycheck-python-flake8-executable "python3"
        )
  (set-face-foreground 'flycheck-error "#FF0461")
  )

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
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
  :bind (("C-c m c" . mc/edit-lines)
	 ("C-c m k" . mc/mark-all-like-this)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (tide))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
