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
  :ensure nil
  :preface
  (defvar fp/indent-width 2)
  :init
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
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-hl-line-mode t))

(use-package "startup"
  :ensure nil
  :init
  (setq inhibit-startup-screen t))

(use-package cus-edit
  :ensure nil
  :init
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
  :init
  (setq auto-revert-check-vc-info t)
  :config
  (global-auto-revert-mode))

(use-package mwheel
  :ensure nil
  :init
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
  :init
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


  (when (member "Code New Roman" (font-family-list))
    (message "Font exists on system")
    (set-frame-font "Code New Roman" t t))

  :config
  (blink-cursor-mode -1))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-type 'relative))

(use-package dired
  :ensure nil
  :init
  (setq delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'startup)
;;; startup.el ends here
