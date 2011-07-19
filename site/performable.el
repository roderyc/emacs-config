;;; Disable unused UI elements.
;;; Be careful to do it in a way that doesn't load them just for them to be
;;; disabled.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (featurep 'menu-bar-mode)
    (menu-bar-mode nil))

(setq-default exec-path
  '("/Users/roderic/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin" "/usr/local/sbin"
    "/Applications/Emacs.app/Contents/MacOS/bin" ))

;;; Make command meta.
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(set-frame-font "Menlo Regular 13")

;;; Set the indentation when editing Python to 2.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default python-indent 2)))

;;; Set indentation in html to 2
(add-hook 'html-mode-hook
          (lambda ()
            (setq-default sgml-basic-offset 2)))

;;; Set the column to indent on to 80
(setq-default fill-column 100)

;;; Use the gnu style when editing c-ish languages.
(add-hook 'c-initialization-hook
          (lambda ()
            (setq-default c-default-style "gnu")))

;;; Set tab width to 2.
(setq-default tab-width 2)
