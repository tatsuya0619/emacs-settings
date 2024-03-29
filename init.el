;;; Init.el --- Initialization file for Emacs
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(electric-pair-mode t)
(column-number-mode 1)
(global-auto-revert-mode 1)
(setq select-enable-clipboard t)
(setq require-final-newline t)
(set-face-background 'mode-line "color-22")

(defun shrink-window-horizontally-by4()
  "Shrink holizon."
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'shrink-window-horizontally)
    )
  )

(defun enlarge-window-horizontally-by4()
  "Enlarge holizon."
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'enlarge-window-horizontally)
    )
  )

(defun shrink-window-by4()
  "Enlarge."
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'shrink-window)
    )
  )

(defun enlarge-window-by4()
  "Shrink."
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'enlarge-window)
    )
  )

(use-package spaceline
  :ensure t
    )

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-minor-modes-off)
  )

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;     (add-hook 'after-init-hook 'benchmark-init/deactivate))

(bind-keys*
 ("C-t" . other-window)
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c b" . org-switchb)
 ("C-z" . help-for-help)
 ("M-h" . backward-kill-word)
 ("S-C-<left>" . shrink-window-horizontally-by4)
 ("S-C-<right>" . enlarge-window-horizontally-by4)
 ("S-C-<down>" . shrink-window-by4)
 ("S-C-<up>" . enlarge-window-by4)
 )
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

(use-package dracula-theme
  :ensure t
  )


(use-package hydra
  :ensure t)

;; (defun my-god-mode-update-modeline ()
;;   "Toggle modeline."
;;   (let ((limited-colors-p (> 257 (length (defined-colors)))))
;;     (cond (god-local-mode (progn
;;                             (set-face-background 'mode-line (if limited-colors-p "color-90" "color-90"))
;;                             (set-face-background 'mode-line-inactive (if limited-colors-p "color-90" "color-90"))))
;;           (t (progn
;;                (set-face-background 'mode-line (if limited-colors-p "color-22" "color-22"))
;;                (set-face-background 'mode-line-inactive (if limited-colors-p "color-22" "color-22")))))))

;; (use-package god-mode
;;   :ensure t
;;   :config
;;   :bind
;;   (
;;    :map global-map
;;    ("C-j" . god-mode-all)
;;    :map god-local-mode-map
;;    ("j" . god-local-mode)
;;    )
;;   :hook
;;   (god-mode-enabled . my-god-mode-update-modeline)
;;   (god-mode-disabled . my-god-mode-update-modeline)
;;   )

;;major-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  )

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
 )

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package nasm-mode
  :ensure t
  :mode "\\.asm\\'")

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  )

(use-package tide
  :ensure t
  :hook ((typescript-mode . tide-setup)
         (before-save . tide-format-before-save))
  )
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

(use-package prettier
  :ensure t
  :hook
  ((web-mode) . prettier-mode)
)

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  )

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook
  ((c-mode python-mode rust-mode typescript-mode go-mode) . lsp)
  :commands (lsp)
  :bind
  ("M-r" . lsp-rename)
  :config
  (setq lsp-prefer-flymake nil)
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq-default lsp-pyls-plugins-pydocstyle-enabled t)
  (setq-default lsp-pyls-plugins-pylint-enabled nil)
  )

;; sentence wrap of lsp-ui-doc doesn't work correctly when we split views.
(use-package lsp-ui
  :ensure t
  :hook
  ((lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t)
  )

(use-package realgud
  :ensure t)

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

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode)
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (set-face-foreground 'flycheck-error "#FF0461")
  )

(use-package flyspell
  :ensure t
  :hook
   ;;((prog-mode). flyspell-prog-mode)
   ((text-mode) . flyspell-mode)
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
  :ensure t
  :config
  (yas-global-mode 1)
  )

(use-package multiple-cursors
  :ensure t
  :bind (
         ("C-c m c" . mc/edit-lines)
         ("C-c m k" . mc/mark-all-like-this)
         )
  )


;; Use bind-key is better? But it didn't work :(
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-mode-map (kbd "C-c C-j") 'term-char-mode)
             (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
             ))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (add-to-list 'term-unbind-key-list "C-t" "C-c")
  :bind
  ("C-c s" . multi-term)
  )

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
    (ace-window async avy benchmark-init bind-key bui company dap-mode dash dash-functional dockerfile-mode dracula-theme epl f flycheck flycheck-rust format-all git-commit git-gutter go-mode god-mode helm helm-core helm-descbinds helm-file-preview helm-projectile ht hydra js2-mode language-id load-relative loc-changes lsp-mode lsp-treemacs lsp-ui lv magit markdown-mode mini-modeline multi-term multiple-cursors nasm-mode pfuture pkg-info popup posframe powerline projectile realgud realgud-lldb spaceline spinner test-simple tide transient treemacs typescript-mode use-package virtualenvwrapper web-mode with-editor yaml-mode yasnippet spaceline-config sphinx-doc tree-mode helm-rg org-preview-html-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "brightmagenta" :underline nil))))
 '(helm-selection-line ((t (:background "brightmagenta"))))
 '(line-number ((t (:background "unspecified-bg" :foreground "green" :slant italic))))
 '(lsp-face-highlight-read ((t (:inherit highlight :background "color-92" :underline t))))
 '(lsp-face-highlight-textual ((t (:inherit highlight :background "color-92"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "white")))))
