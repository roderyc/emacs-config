;;; Disable unused UI elements.
;;; Be careful to do it in a way that doesn't load them just for them to be
;;; disabled.
(if (featurep 'tool-bar)
    (tool-bar-mode nil))
(if (featurep 'tooltip)
    (tooltip-mode nil))
(if (featurep 'menu-bar)
    (menu-bar-mode nil))

;;; Set the indentation when editing python to 4.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default python-indent 4)))

;;; Set indentation in html to 4
(add-hook 'html-mode-hook
          (lambda ()
            (setq-default sgml-basic-offset 4)))

(require 'ipython)

;;; Delete trailing whitespace before saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Set the font to Monospace 10 pt.
(set-frame-font "Monospace 10")

;;; Set the column to indent on to 80
(setq-default fill-column 80)
