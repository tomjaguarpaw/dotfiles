(load "~/Config/Emacs/keybindings")
(load "~/Config/Emacs/latex")
; (load "~/Config/Emacs/show-whitespace-mode.el")
(load "~/Config/Emacs/whitespace.el")
(load "~/Config/Emacs/hi2.el")

(add-hook 'haskell-mode-hook 'turn-on-hi2)

; http://steve.yegge.googlepages.com/effective-emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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
