;;; startup --- Startup settings and built in package setup
;;; Commentary:
;;; Code:

;; Straight.el initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'git)

(use-package emacs
  :straight t
  :preface
  (defvar fp/indent-width 2)
  :config
  (setq default-directory "~/")
  (setq frame-resize-pixelwise t)
  (setq load-prefer-newer t)
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups"))))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width fp/indent-width)
  (fset 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-hl-line-mode t))

(use-package "startup"
  :ensure nil
  :config
  (setq inhibit-startup-screen t))

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file "~/.emacs.d/to-be-dumped.el"))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package simple
  :ensure nil
  :config
  (line-number-mode +1)
  (column-number-mode +1))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :ensure nil
  :config
  (defun frame-title-format ()
    "Return frame title with current project name, where applicable."
    (concat
     "emacs - "
     (when (and (bound-and-true-p projectile-mode)
                (projectile-project-p))
       (format "[%s] - " (projectile-project-name)))
     (let ((file buffer-file-name))
       (if file
           (abbreviate-file-name file)
         "%b"))))

  (setq-default frame-title-format '((:eval (frame-title-format))))
  ;; (setq initial-frame-alist '((fullscreen . maximized)))
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (set-frame-parameter (selected-frame) 'alpha 85)
  ;; (add-to-list 'default-frame-alist '(alpha . 85))

  (blink-cursor-mode -1)

  (when (member "Fira Code" (font-family-list))
    (message "Font exists on system")
    (set-frame-font "Fira Code" t t)))

;; (use-package ediff
;;   :ensure nil
;;   :custom
;;   (ediff-split-window-function #'split-window-horizontally))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-type 'relative))

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'startup)
;;; startup.el ends here
