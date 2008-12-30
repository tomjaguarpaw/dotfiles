(load "~/Config/Emacs/keybindings")
(load "~/Config/Emacs/latex")
; (load "~/Config/Emacs/show-whitespace-mode.el")
(load "~/Config/Emacs/whitespace.el")

; http://steve.yegge.googlepages.com/effective-emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
