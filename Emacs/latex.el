(defun my-latex-mode-hook ()
        (reftex-mode)
        (auto-fill-mode "yes")
	(font-lock-mode)
)

; latex-mode-hook is for the emacs builtin latex mode.
; LaTeX-mode-hook is for AUCTeX's latex mode.
;
;     https://emacs.stackexchange.com/a/7469
(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)
(add-hook 'latex-mode-hook 'my-latex-mode-hook)
