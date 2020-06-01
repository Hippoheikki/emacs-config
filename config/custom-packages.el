;;; custom-packages --- Custom packages
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'sidebar-mode)
  (evil-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

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
  :hook (with-editor-mode-hook . evil-insert-state))

(use-package projectile
  :straight t
  :init
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)
  (setq projectile-git-submodule-command nil)
  :config
  (projectile-mode))

(use-package helm
  :straight t)

(use-package helm-projectile
  :straight t)

(use-package helm-rg
  :straight t)

(use-package swiper-helm
  :straight t)

(use-package helm-lsp
  :straight t)

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  :config
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil))

(use-package company-lsp
  :straight t
  :init
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-enable-recompletion t)
  :config
  (add-to-list 'company-backends #'company-lsp))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package dashboard
  :straight t
  :init
  (setq dashboard-startup-banner '2)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 8)
                          (recents . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package awesome-tab
  :straight (awesome-tab :type git :host github :repo "manateelazycat/awesome-tab")
  :init
  (setq awesome-tab-height 100)
  :config
  (awesome-tab-mode t))

(use-package telephone-line
  :straight t
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  :config
  (telephone-line-mode t))

(use-package neotree
  :straight t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package git-gutter
  :straight t
  :init
  (setq git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :straight t
  :init
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :config
  (global-git-gutter-mode +1))

(use-package rainbow-mode
  :straight t
  :hook (web-mode . rainbow-mode))

(use-package color-identifiers-mode
  :straight t
  :hook (after-init-hook . global-color-identifiers-mode))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package all-the-icons
  :straight t
  :init
  (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package highlight-symbol
  :straight t
  :hook (prog-mode . highlight-symbol-mode)
  :init
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-escape-sequences
  :straight t
  :hook (prog-mode . hes-mode))

;; Language support
(use-package format-all
  :straight t)

(use-package flycheck
  :straight t
  :init
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-posframe
  :straight t
  :hook (flycheck-mode . flycheck-posframe-mode)
  :init
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
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-clients-typescript-log-verbosity "debug"
        lsp-clients-typescript-plugins
        (vector
         (list :name "typescript-tslint-plugin"
               :location "<home>/.nvm/versions/node/v13.12.0/lib/node_modules/typescript-tslint-plugin/")))
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t)))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :config
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
