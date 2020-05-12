;;; startup --- Startup settings and built in package setup
;;; Commentary:
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar fp/gc-cons-threshold 100000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold fp/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold fp/gc-cons-threshold)))


(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package emacs
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-hl-line-mode t)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups"))))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq create-lockfiles nil)
  (setq ring-bell-function 'ignore)
  (setq-default line-spacing 5
                indent-tabs-mode nil))

(use-package "startup"
  :ensure nil
  :custom
  (inhibit-startup-screen t))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file "~/.emacs.d/to-be-dumped.el"))

(use-package simple
  :ensure nil
  :config
  (line-number-mode +1)
  (column-number-mode +1))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :custom
  (confirm-kill-processes nil)
  (make-backup-files nil))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.4))

(use-package cc-vars
  :ensure nil
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "k&r"))))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :ensure nil
  :custom
  (initial-frame-alist '((fullscreen . maximized)))
  (ns-use-proxy-icon nil)
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
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (blink-cursor-mode -1)

  (when (member "Hack" (font-family-list))
    (message "Font exists on system")
    (set-frame-font "Hack-12" t t)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function #'split-window-horizontally))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'startup)
;;; startup.el ends here
