(defun kl/org-mode-hook ()
  (org-indent-mode)
  (visual-line-mode 1)
  (display-line-numbers-mode 0)
  (kl/org-variable-pitch-mode 1)
  (visual-fill-column-mode 1))

(defun kl/rust-mode-hook ()
  (setq indent-tabs-mode nil
	tab-width 4
	c-basic-offset 4
	fill-column 100))

(defun kl/eglot-managed-mode-hook ()
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
  (fset #'jsonrpc--log-event #'ignore)
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(defun kl/org-variable-pitch-mode (&optional arg)
  (require 'face-remap)
  (buffer-face-mode-invoke 'org-default (or arg t)))

(defun kl/org-set-faces ()
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'org-default nil :font "MS Sans Serif" :height 80))
  (when (eq system-type 'darwin)
    (set-face-attribute 'org-default nil :font "Helvetica Neue" :height 100))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'org-default nil :font "Inter" :height 100)))

(defun kl/org-roam-insert-imm (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
	(org-roam-capture-templates
	 (list (append (car org-roam-capture-templates)
		       '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun kl/compile-treesit-grammars ()
  (interactive)
  (let ((langs (mapcar 'car treesit-language-source-alist)))
    (dolist (lang langs)
      (treesit-install-language-grammar lang))))

(defun kl/set-faces ()
  (set-face-attribute 'default nil :font "Berkeley Mono" :height 100)
  (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :height 100)
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'variable-pitch nil :font "MS Sans Serif" :height 80))
  (when (eq system-type 'darwin)
    (set-face-attribute 'variable-pitch nil :font "Helvetica Neue" :height 100))
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'variable-pitch nil :font "Inter" :height 100)))
