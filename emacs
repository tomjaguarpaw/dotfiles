
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/Config/Emacs/keybindings")
(load "~/Config/Emacs/latex")
; (load "~/Config/Emacs/show-whitespace-mode.el")
(load "~/Config/Emacs/whitespace.el")
(load "~/Config/Emacs/hi2.el")
(load "~/Config/Emacs/attrap-20180218.1243.el")
(load "~/Config/Emacs/lcr-20180224.1243.el")
(load "~/Config/Emacs/dante.el")

(add-hook 'haskell-mode-hook 'turn-on-hi2)
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

; http://steve.yegge.googlepages.com/effective-emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(show-paren-mode 1)
(column-number-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 112 :width normal)))))

(setq ring-bell-function 'ignore)
