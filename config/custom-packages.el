;;; custom-packages --- Custom packages
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  :config
  (load-theme 'doom-solarized-dark t))

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-C-i-jump nil
        evil-want-keybinding nil
        evil-search-module 'isearch
        evil-ex-search-vim-style-regexp t)
  :config
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-blame-mode)
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  (evil-mode))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-magit)

(use-package magit
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package projectile
  :config
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (add-to-list 'projectile-globally-ignored-directories "*libs")
  (projectile-mode))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :custom
  (counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package ivy
  :hook (after-init . ivy-mode)
  :custom
  (ivy-display-style nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((counsel-rg            . ivy--regex-plus)
                           (counsel-projectile-rg . ivy--regex-plus)
                           (counsel-ag            . ivy--regex-plus)
                           (counsel-projectile-ag . ivy--regex-plus)
                           (swiper                . ivy--regex-plus)
                           (t                     . ivy--regex-fuzzy))))

(use-package ivy-rich
    :preface
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (all-the-icons-icon-for-mode major-mode)))
    :init
    (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-switch-buffer-icon (:width 5))
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 35))
              (ivy-rich-counsel-function-docstring (:width 44 :face font-lock-doc-face))))))
    :config
    (ivy-rich-mode +1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center))
        ivy-posframe-height-alist '((swiper . 30)
                                    (t . 10))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 80)
  (ivy-posframe-mode +1))

(use-package swiper
  :after ivy
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t))

(use-package prescient
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper
          counsel-grep
          counsel-rg
          counsel-projectile-rg
          ivy-switch-buffer
          counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode +1))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "SPC") nil)))

(use-package company-posframe
  :custom
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil)
  :config
  (company-posframe-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(use-package lsp-mode
  :hook ((js-mode         ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          ) . lsp)
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (add-hook 'lsp-ui-mode-hook
            (defun +lsp-init-ui-flycheck ()
              "Sets up flycheck-mode."
              (require 'flycheck nil t)
              (require 'lsp-ui-flycheck)
              (lsp-ui-flycheck-enable t)))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy)

(use-package typescript-mode
  :after (company flycheck lsp-ui)
  :config
  (flycheck-add-next-checker 'lsp-ui 'typescript-tslint))

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto))

(use-package flycheck
  :custom
  (flycheck-display-errors 0.25)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (global-flycheck-mode +1))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-left-corner)
  (flycheck-posframe-warning-prefix "⚠ ")
  (flycheck-posframe-info-prefix "... ")
  (flycheck-posframe-error-prefix "✕ ")
  :config
  (add-hook 'flycheck-posframe-inhibit-functions
            #'company--active-p
            #'evil-insert-state-p
            #'evil-replace-state-p))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon t)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-modal-icon nil)
  :config
  (doom-modeline-def-modeline 'simple
    '(bar modals buffer-info buffer-position)
    '(misc-info lsp buffer-encoding major-mode vcs))
  (defun fp/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'simple 'default))
    (add-hook 'doom-modeline-mode-hook 'fp/setup-custom-doom-modeline))

(use-package format-all)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package rainbow-mode
  :hook (web-mode . rainbow-mode))

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package default-text-scale)

(provide 'custom-packages)
;;; custom-packages.el ends here
