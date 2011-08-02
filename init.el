;;; OS level copy / paste
(setq x-select-enable-clipboard t)

(setq make-backup-files nil)

;;; Start the emacs server so that emacsclient can be used.
(server-start)

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

;;; Allow narrowing without showing a warning.
(put 'narrow-to-region 'disabled nil)

;;; Same for upcase-region
(put 'upcase-region 'disabled nil)

;;; And downcase-region
(put 'downcase-region 'disabled nil)

;;; Indent with spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; Load markdown-mode on markdown files.
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
             (cons "\\.mdwn$" 'markdown-mode))

;;; Load textile-mode on textile files.
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;; Execute a command without having to use meta.
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;; Insert a λ.
(global-set-key "\C-x/" '(lambda () (interactive) (insert #x3bb)))

;;; Global color theme stuff.
(require 'color-theme)

;;; Tango color theme.
(autoload 'color-theme-tango "color-theme-tango"
  "Tango color theme for emacs"
  t)
(color-theme-tango)

;;; Handle SGR control sequences in output. Allows colored text from e.g. a unix
;;; command.
(ansi-color-for-comint-mode-on)

;;; Auto-Fill for these modes.
(dolist (hook '(text-mode-hook
                tuareg-mode-hook
                python-mode-hook
                scheme-mode-hook))
  (add-hook hook 'turn-on-auto-fill))

(load-library (read-string "Which profile to load?: "))
