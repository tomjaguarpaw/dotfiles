;; Translate `C-h' to <DEL>.
;;     (keyboard-translate ?\C-h ?\C-?)
     
;; Translate <DEL> to `C-h'.
;;     (keyboard-translate ?\C-? ?\C-h)

;; stuff to rebind the help key - from dug's .emacs
;; Make CTRL-h delete the previous character. Normally, this is "help"
(global-set-key "\C-h" 'delete-backward-char)
(define-key global-map "\C-h" 'backward-delete-char)

(global-set-key "\C-t" nil)

;; Make sure CTRL-h works in searches, too.
(setq search-delete-char (string-to-char "\C-h"))

(global-set-key [(control ?x) (control ?c)]
                (lambda
		  ()
		  (interactive)
		  (message "Use M-x save-buffers-kill-terminal")))

(setq visible-cursor nil)
