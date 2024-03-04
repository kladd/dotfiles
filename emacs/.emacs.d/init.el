(require 'package)
(require 'use-package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package emacs
  :init
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "https://elpa.gnu.org/packages/")
			   ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
	use-package-always-ensure t)
  (setq make-backup-files nil
	create-lockfiles nil
	custom-file (make-temp-file "emacs-custom-"))
  (setq vc-follow-symlinks t)
  (setq initial-buffer-choice t)
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui t
	modus-themes-bold-constructs t
	modus-themes-italic-constructs t)
  (setq x-underline-at-descent-line nil)
  (setq scroll-conservatively 100
	scroll-margin 4)
  (setq treesit-language-source-alist
	'((rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (make . ("https://github.com/alemuller/tree-sitter-make"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))))
  (setq frame-title-format '("%b - " invocation-name "@" system-name))
  :config
  (load-theme 'modus-vivendi t)
  (electric-pair-mode 1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (global-auto-revert-mode 1)
  (column-number-mode 1)
  (global-display-line-numbers-mode 1)
  (keymap-set global-map "<escape>" 'keyboard-escape-quit)
  (set-face-attribute 'default nil
		      :font "Berkeley Mono" :height 100)
  (set-face-attribute 'fixed-pitch nil
		      :font "Berkeley Mono" :height 100)
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'variable-pitch nil
			:font "Helvetica" :height 80)
    (pixel-scroll-precision-mode 1)))
    
(use-package vertico
  :config (vertico-mode))

(use-package marginalia
  :config (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  :init (global-corfu-mode))

(use-package spacious-padding
  :config (spacious-padding-mode 1))

(use-package orderless
  :init (setq completion-styles '(orderless basic)))

(use-package which-key
  :init
  (setq which-key-idle-delay 400
	which-key-separator "  " 
	which-key-add-column-padding 1
	which-key-max-display-columns 3
	which-key-max-description-length 40)
  :config
  (which-key-mode))

(use-package org
  :init (setq org-hide-emphasis-markers t)
  :config
  (set-face-attribute 'org-default nil
		      :font "Iosevka Comfy Motion Duo"
		      :height 110)
  :hook (org-mode . kl/org-mode-hook))

(use-package org-roam
  :custom
  (org-roam-directory "~/log")
  (org-roam-completion-everywhere t)
  :bind (("C-c i" . kl/org-roam-insert-imm))
  :config
  (org-roam-setup))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t))

(use-package flycheck)

(use-package rust-ts-mode
  :init
  (setq rust-format-on-save t
	rust-rustfmt-switches '("+nightly --all")
	eldoc-echo-area-use-multiline-p nil)
  (setq exec-path (append exec-path (list (expand-file-name "~/.cargo/bin"))))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package evil-leader
  :config
  (require 'org-roam-dailies)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" #'project-find-file
    "b" #'switch-to-buffer
    "d" org-roam-dailies-map)
  (global-evil-leader-mode))

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-define-key 'motion org-mode-map (kbd "C-\]")
    'org-open-at-point)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(defun kl/org-mode-hook()
  (org-indent-mode)
  (visual-line-mode 1)
  (display-line-numbers-mode 0)
  (kl/org-variable-pitch-mode 1)
  (visual-fill-column-mode 1))

(defun kl/org-variable-pitch-mode (&optional arg)
  (require 'face-remap)
  (buffer-face-mode-invoke 'org-default (or arg t)))

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
