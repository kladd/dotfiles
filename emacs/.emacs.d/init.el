(require 'package)
(require 'use-package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(load (locate-user-emacs-file "kl.el") :no-message)

(use-package emacs
  :init
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("gnu" . "https://elpa.gnu.org/packages/")
			   ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
	use-package-always-ensure t)
  (setq make-backup-files nil
	create-lockfiles nil
	auto-save-default nil
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
  (setq major-mode-remap-alist '((python-mode . python-ts-mode)))
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
  (unless (eq system-type 'darwin)
    (pixel-scroll-precision-mode 1))
  (kl/set-faces))
    
(use-package vertico
  :config (vertico-mode))

(use-package marginalia
  :config (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
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
  :config (kl/org-set-faces)
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

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . eglot-ensure)
	 (rust-mode . kl/rust-mode-hook))
  :init (setq rust-mode-treesitter-derive t
	      rust-format-on-save t
	      rust-rustfmt-switches '("+nightly")
	      eldoc-echo-area-use-multiline-p nil))

(use-package python-mode
  :mode "\\.py\\'"
  :hook ((python-ts-mode . eglot-ensure)))

(use-package flymake-clippy
  :hook (rust-mode . flymake-clippy-setup-backend))

(use-package eglot
  :hook (eglot-managed-mode . kl/eglot-managed-mode-hook)
  :bind (("<M-return>" . eglot-code-actions))
  :config (add-to-list 'eglot-stay-out-of 'flymake))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-evil-setup t)
  (require 'org-roam-dailies)
  (general-create-definer kl/leader-keys :prefix "SPC"))

(kl/leader-keys
  :states '(normal visual)
  :keymaps 'override
  "f" #'project-find-file
  "b" #'switch-to-buffer
  "k" #'kill-buffer
  "l" #'dired
  "d" org-roam-dailies-map)

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
