;;; Disable unused UI elements.
;;; Be careful to do it in a way that doesn't load them just for them to be
;;; disabled, slowing down startup time.
(if (featurep 'tool-bar)
    (tool-bar-mode nil))
(if (featurep 'tooltip)
    (tooltip-mode nil))
(if (featurep 'menu-bar)
    (menu-bar-mode nil))

;;; OS level copy / paste
(setq x-select-enable-clipboard t)

;;; Email address
(setq user-mail-address "roderic@ccs.neu.edu")

(setq make-backup-files nil)

;;; Set the column to indent on to 80
(setq-default fill-column 80)

;;; Start the emacs server so that emacsclient can be used.
(server-start)

;;; Auto-Fill for these modes.
(dolist (hook '(text-mode-hook
                tuareg-mode-hook
                python-mode-hook
                scheme-mode-hook))
  (add-hook hook 'turn-on-auto-fill))

;;; Add the site directory to load path.
(add-to-list 'load-path "~/.emacs.d/site/")

;;; Paredit
(require 'paredit)
(dolist (hook '(scheme-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;;; Scheme48
(autoload 'scheme48-mode "scheme48"
  "Major mode for editing scheme with scheme48." t)

;;; Zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; Tuareg
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
  (add-to-list 'completion-ignored-extensions ext))

;;; Show column numbers in the mode line.
(column-number-mode t)

;;; Highlight open and close parenthesis when the point is on them.
(show-paren-mode t)

;;; Convenient way to open files and switch buffers.
(ido-mode t)

;;; Set the font to Monospace 10 pt.
(set-frame-font "Monospace 10")

;;; Allow narrowing without showing a warning.
(put 'narrow-to-region 'disabled nil)

;;; Same for upcase-region
(put 'upcase-region 'disabled nil)

;;; And downcase-region
(put 'downcase-region 'disabled nil)

;;; Indent with spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; Load markdown-mode on markdown files.
(add-to-list 'auto-mode-alist
             (cons "\\.mdwn$" 'markdown-mode))

;;; Execute a command without having to use meta.
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;; Delete trailing white space when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Set the indentation when editing python to 4.
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default python-indent 4)))

;;; Use the stroustrup style when editing c.
(add-hook 'c-initialization-hook
          (lambda ()
            (setq-default c-default-style "stroustrup")))

;;; Set indentation in html to 4
(add-hook 'html-mode-hook
          (lambda ()
            (setq-default sgml-basic-offset 4)))

;;; Global color theme stuff.
(require 'color-theme)
(setq color-theme-is-global t)

;;; Tango color theme.
(autoload 'color-theme-tango "color-theme-tango"
  "Tango color theme for emacs"
  t)
(color-theme-tango)

;;; Handle SGR control sequences in output. Allows colored text from e.g. a unix
;;; command.
(ansi-color-for-comint-mode-on)

(require 'ipython)
