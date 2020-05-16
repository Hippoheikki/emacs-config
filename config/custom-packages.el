;;; custom-packages --- Custom packages
;;; Commentary:
;;; Code:

;; Editor functionality related packages
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-magit)

(use-package magit
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

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
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 15)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg            . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (swiper                . ivy--regex-plus)
          (t                     . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package company
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

(use-package format-all)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package default-text-scale)


;; Editor visual related packages
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-outrun-electric t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-modal-icon nil)
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-enable-word-count t)
  (doom-modeline-def-modeline 'simple
    '(modals buffer-info-simple buffer-position indent-info word-count selection-info)
    '(misc-info lsp buffer-encoding major-mode vcs checker))
  (defun fp/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'simple 'default))
  (add-hook 'doom-modeline-mode-hook 'fp/setup-custom-doom-modeline))

(use-package dashboard
  :custom
  (dashboard-startup-banner '2)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((projects . 8)
                     (recents . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :demand
  :hook
  (dired-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-height 32)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  (centaur-tabs-style "bar")
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons))

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

(use-package ivy-rich
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(use-package rainbow-mode
  :hook (web-mode . rainbow-mode))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))


;; Packages related to language support
(use-package lsp-mode
  :hook ((js-mode         ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          lua-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-prefer-capf t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :commands company-lsp
  :config
  (setq company-lsp-cache-candidates 'auto)
  (push 'company-lsp company-backends)
  (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t)))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-posframe
  :after flycheck
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

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode)))

(use-package lua-mode)

(use-package typescript-mode)

(use-package yaml-mode)

(use-package rustic)

(provide 'custom-packages)
;;; custom-packages.el ends here
