;;; init.el --- Emacs init file
;; Author: Frans Paasonen
;;; Commentary:
;; Installation:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus --without-spacemacs-icon --HEAD --with-jansson
;;; Code:

(load-file (concat user-emacs-directory "config/startup.el"))
(load-file (concat user-emacs-directory "config/custom-packages.el"))
(load-file (concat user-emacs-directory "config/keybinds.el"))

(provide 'init)
;;; init.el ends here
