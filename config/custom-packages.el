;;; custom-packages --- Custom packages
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (evil-mode))

(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

(use-package evil-collection
  :straight t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode +1))

(use-package evil-magit
  :straight t)

(use-package magit
  :straight t
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package projectile
  :straight t
  :config
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)
  (projectile-mode))

(use-package helm
  :straight t)

(use-package helm-projectile
  :straight t)

(use-package helm-rg
  :straight t)

(use-package swiper-helm
  :straight t)

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "SPC") nil)))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package dashboard
  :straight t
  :config
  (setq dashboard-startup-banner '2)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 8)
                     (recents . 5)))
  (dashboard-setup-startup-hook))

(use-package awesome-tab
  :straight (awesome-tab :type git :host github :repo "manateelazycat/awesome-tab")
  :config
  (setq awesome-tab-height 100)
  (awesome-tab-mode t))

(use-package telephone-line
  :straight t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode t))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons))

(use-package git-gutter
  :straight t
  :custom
  (git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :straight t
  :config
  (global-git-gutter-mode +1)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package rainbow-mode
  :straight t
  :hook (web-mode . rainbow-mode))

(use-package color-identifiers-mode
  :straight t
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package all-the-icons
  :straight t
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package highlight-symbol
  :straight t
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-escape-sequences
  :straight t
  :hook (prog-mode . hes-mode))


;; Language support
(use-package format-all
  :straight t)

(use-package flycheck
  :straight t
  :config
  (setq flycheck-display-errors-delay 0.25)
  (global-flycheck-mode))

(use-package flycheck-posframe
  :straight t
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-position 'window-bottom-left-corner)
  (setq flycheck-posframe-warning-prefix "⚠ ")
  (setq flycheck-posframe-info-prefix "... ")
  (setq flycheck-posframe-error-prefix "✕ ")
  (add-hook 'flycheck-posframe-inhibit-functions
            #'company--active-p
            #'evil-insert-state-p
            #'evil-replace-state-p))

(use-package lsp-mode
  :straight t
  :hook ((js-mode         ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          lua-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-clients-typescript-log-verbosity "debug"
        lsp-clients-typescript-plugins
        (vector
         (list :name "typescript-tslint-plugin"
               :location "<home>/.nvm/versions/node/v13.12.0/lib/node_modules/typescript-tslint-plugin/"))))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode)))

(use-package lua-mode
  :straight t)

(use-package typescript-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package rustic
  :straight t)

(provide 'custom-packages)
;;; custom-packages.el ends here
