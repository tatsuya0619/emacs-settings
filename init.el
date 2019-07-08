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
;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;;global settings
(setq inhibit-splash-screen t)
(global-linum-mode t)
(show-paren-mode t)
(setq backup-inhibited t)
(setq scroll-step 1)
(menu-bar-mode -1)
(define-key key-translation-map [?\C-h] [?\C-?])

;;major-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package nasm-mode
  :ensure t
  :mode "\\.asm\\'")

;;minor-mode
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  ;for python3 syntax
  (setq flycheck-python-pycompile-executable "python3")
  (set-face-foreground 'flycheck-error "#FF0461")
  )


(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
	 ("C-c m k" . mc/mark-all-like-this)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
