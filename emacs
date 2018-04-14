
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/Config/Emacs/keybindings")
(load "~/Config/Emacs/latex")
; (load "~/Config/Emacs/show-whitespace-mode.el")
(load "~/Config/Emacs/whitespace.el")
(load "~/Config/Emacs/attrap-20180218.1243.el")
(load "~/Config/Emacs/lcr-20180224.1243.el")
(load "~/Config/Emacs/dante.el")

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

; http://steve.yegge.googlepages.com/effective-emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

(show-paren-mode 1)
(column-number-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dante-repl-command-line-methods-alist
   (quote
    ((styx closure
	   (t)
	   (root)
	   (dante-repl-by-file root
			       (quote
				("styx.yaml"))
			       (quote
				("styx" "repl" dante-target))))
     (nix closure
	  (t)
	  (root)
	  (dante-repl-by-file root
			      (quote
			       ("shell.nix" "default.nix"))
			      (quote
			       ("nix-shell" "--run"
				(concat "cabal repl "
					(or dante-target "")
					" --builddir=dist/dante")))))
     (mafia closure
	    (t)
	    (root)
	    (dante-repl-by-file root
				(quote
				 ("mafia"))
				(quote
				 ("mafia" "repl" dante-target))))
     (new-build closure
		(t)
		(root)
		(if
		    (or
		     (directory-files root nil ".+\\.cabal$")
		     (file-exists-p "cabal.project"))
		    (progn
		      (quote
		       ("cabal" "new-repl" dante-target "--builddir=dist/dante")))))
     (bare closure
	   (t)
	   (_)
	   (quote
	    ("cabal" "repl" dante-target "--builddir=dist/dante"))))))
 '(load-home-init-file t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Inconsolata")))))

(setq ring-bell-function 'ignore)
