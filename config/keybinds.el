;;; keybinds --- Custom keybinds
;;; Commentary:
;;; Code:

(defun fp/reload-conf ()
  "Evaluate init.el file."
  (interactive)
  (load (expand-file-name (concat user-emacs-directory "init.el"))))

(defun fp/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package general
  :demand
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'override
   "SPC" '(counsel-M-x :which-key "Extended command")

   ;; File actions
   "f"  '(:ignore t :which-key "Files")
   "ff" '(counsel-find-file :which-key "Find file")
   "fp" '(projectile-find-file :which-key "Find file in project")
   "fs" '(save-buffer :which-key "Save buffer")
   "fS" '(save-some-buffers :which-key "Save all buffers")

   ;; Projectile actions
   "p"  '(:ignore t :which-key "Project management")
   "pp" '(projectile-switch-project :which-key "Switch project")
   "pf" '(projectile-find-file :which-key "Find file in project")
   "pk" '(projectile-kill-buffers :which-key "Kill project buffers")
   "ps" '(projectile-ripgrep :which-key "Search project")
   "pi" '(projectile-invalidate-cache :which-key "Invalidate cache")
   "pa" '(projectile-add-known-project :which-key "Add project")
   "pr" '(projectile-remove-known-project :which-key "Remove project")

   ;; Search actions
   "s"  '(:ignore t :which-key "Search")
   "ss" '(swiper :which-key "Swiper (ivy)")
   "sp" '(projectile-ripgrep :which-key "Search project")

   ;; Dired actions
   "d"  '(:ignore t :which-key "Dired")
   "dd" '(dired :which-key "Open dired")
   "dj" '(dired-jump :which-key "Jump to file in dired")

   ;; Git actions
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "Magit status")
   "gb" '(magit-blame :which-key "Magit blame")

   ;; Buffer actions
   "b"  '(:ignore t :which-key "Buffers")
   "bb" '(ivy-switch-buffer :which-key "Buffer list (ivy)")
   "bk" '(:ignore t :which-key "Buffer kill actions")
   "bkc" '(kill-current-buffer :which-key "Kill buffer")
   "bko" '(fp/kill-other-buffers :which-key "Kill all other buffers")

   ;; Window actions
   "w"  '(:ignore t :which-key "Window management")
   "wv" '(evil-window-vsplit :which-key "Split window vertically")
   "ws" '(evil-window-split :which-key "Split window horizontally")
   "wc" '(evil-window-delete :which-key "Close window")
   "wC" '(delete-other-windows :which-key "Close other windows")
   "wh" '(evil-window-left :which-key "Move to left window")
   "wj" '(evil-window-down :which-key "Move to down window")
   "wk" '(evil-window-up :which-key "Move to up window")
   "wl" '(evil-window-right :which-key "Move to right window")

   "w <right>" '(centaur-tabs-forward :which-key "Move to next tab")
   "w <left>"  '(centaur-tabs-backward :which-key "Move to previous tab")

   ;; Code actions
   "c"  '(:ignore t :which-key "Code actions")
   "cd" '(xref-find-definitions :which-key "Find definitions")
   "cr" '(xref-find-references :which-key "Find references")
   "cs" '(ivy-lsp-workspace-symbol :which-key "Find symbol")
   "cS" '(ivy-lsp-global-workspace-symbol :which-key "Find symbol (global)")
   "cf" '(format-all-buffer :which-key "Format buffer")

   ;; Revert/reload actions
   "r"  '(:ignore t :which-key "Revert/Reload actions")
   "rb" '(revert-buffer :which-key "Revert buffer")
   "rc" '(fp/reload-conf :which-key "Reload config")

   ;; Quit action
   "q"  '(:ignore t :which-key "Quit actions")
   "qq" '(kill-emacs :which-key "Kill emacs")

   "t"  '(:ignore t :which-key "Toggle/settings")
   ;; "tl" '(global-display-line-numbers-mode :which-key "Toggle line number mode")
   "tw" '(global-whitespace-mode :which-key "Toggle whitespace mode")
   "ti" '(default-text-scale-increase :which-key "Increase text scale")
   "td" '(default-text-scale-decrease :which-key "Decrease text scale")
   "tr" '(default-text-scale-reset :which-key "Reset text scale")
   ))

(provide 'keybinds)
;;; keybinds.el ends here
