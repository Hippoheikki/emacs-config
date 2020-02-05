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

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-C-i-jump nil
        evil-want-keybinding nil
        evil-search-module 'evil-search
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
  :diminish
  :config
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-completion-system 'ivy)
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
  (ivy-re-builders-alist '((counsel-rg            . ivy--regex-plus)
                           (counsel-projectile-rg . ivy--regex-plus)
                           (counsel-ag            . ivy--regex-plus)
                           (counsel-projectile-ag . ivy--regex-plus)
                           (swiper                . ivy--regex-plus)
                           (t                     . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil))

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

(use-package typescript-mode)

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto))

(use-package flycheck
  :diminish
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors 0.25)
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-left-corner))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode)))

(use-package format-all
  :preface
  (defun fp/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  (defalias 'format-document #'fp/format-code))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package rainbow-mode
  :hook (web-mode . rainbow-mode))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diminish
  :demand t)

(provide 'custom-packages)
;;; custom-packages.el ends here
