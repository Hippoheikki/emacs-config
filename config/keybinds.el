;;; keybinds --- Custom keybinds
;;; Commentary:
;;; Code:

(defun fp/reload-conf ()
  "Evaluate init.el file."
  (interactive)
  (load (expand-file-name (concat user-emacs-directory "init.el"))))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package general
  :straight t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   :keymaps 'override
   "SPC" '(helm-M-x :which-key "Extended command")

   ;; Git actions
   "g"  '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "Magit status")
   "gb" '(magit-blame :which-key "Magit blame")

   ;; File actions
   "f"  '(:ignore t :which-key "Files")
   "ff" '(helm-find-files :which-key "Find file")
   "fs" '(save-buffer :which-key "Save buffer")
   "fS" '(save-some-buffers :which-key "Save all buffers")

   ;; Buffer actions
   "b"  '(:ignore t :which-key "Buffers")
   "bb" '(helm-buffers-list :which-key "Buffer list")
   "bk" '(kill-current-buffer :which-key "Kill buffer")

   ;; Dired actions
   "d"  '(:ignore t :which-key "Dired")
   "dd" '(dired :which-key "Open dired")
   "dj" '(dired-jump :which-key "Jump to file in dired")

   ;; Projectile actions
   "p"  '(:ignore t :which-key "Project management")
   "pp" '(helm-projectile-switch-project :which-key "Switch project")
   "pf" '(helm-projectile-find-file :which-key "Find file in project")
   "pk" '(projectile-kill-buffers :which-key "Kill project buffers")
   "pi" '(projectile-invalidate-cache :which-key "Invalidate cache")
   "pa" '(projectile-add-known-project :which-key "Add project")
   "pr" '(projectile-remove-known-project :which-key "Remove project")

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

   ;; Code actions
   "c"  '(:ignore t :which-key "Code actions")
   "cd" '(xref-find-definitions :which-key "Find definitions")
   "cr" '(xref-find-references :which-key "Find references")
   "cf" '(format-all-buffer :which-key "Format buffer")
   "cc" '(goto-last-change :which-key "Goto last change")
   "cs" '(xref-find-apropos :which-key "Find symbol in workspace")
   "cS" '(helm-lsp-global-workspace-symbol :which-key "Find symbol in all workspaces")
   "ca" '(helm-lsp-code-actions :which-key "LSP code actions")

   ;; Search actions
   "s"  '(:ignore t :which-key "Search")
   "sp" '(helm-projectile-rg :which-key "Search project")
   "sb" '(swiper :which-key "Search buffer")

   ;; Reload actions
   "r"  '(:ignore t :which-key "Reload actions")
   "rc" '(fp/reload-conf :which-key "Reload config")

   ;; Quit action
   "q"  '(:ignore t :which-key "Quit actions")
   "qq" '(kill-emacs :which-key "Kill emacs")

   ;; Toggle actions / tab actions
   "t"  '(:ignore t :which-key "Toggle/settings")
   "tw" '(global-whitespace-mode :which-key "Toggle whitespace mode")
   "ts" '(dired-sidebar-toggle-sidebar :which-key "Toggle dired sidebar")
   "tl" '(awesome-tab-forward :which-key "Move to next tab")
   "th" '(awesome-tab-backward :which-key "Move to previous tab")
   ))

(provide 'keybinds)
;;; keybinds.el ends here
