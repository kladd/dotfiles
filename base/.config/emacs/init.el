;; -*- lexical-binding: t -*-

(require 'package)
(require 'use-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package emacs
  :ensure nil
  :config
  (setq inhibit-startup-screen t
	initial-buffer-choice t)
  (setq frame-title-format '("%b - " invocation-name "@" system-name))
  (setq scroll-conservatively 100
	scroll-margin 4)
  (setq auto-save-default nil
	make-backup-files nil
	create-lockfiles nil
	custom-file (make-temp-file "emacs-custom-"))
  (setq require-final-newline t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment "UTF-8")

  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (electric-pair-mode)
  (column-number-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (keymap-set global-map "<escape>" 'keyboard-escape-quit)

  (set-face-attribute 'default nil :font "Berkeley Mono" :height 110))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key  evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  :config
  (add-hook 'prog-mode-hook 'corfu-mode))

(use-package consult
  :ensure t)

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion--category-overrides '((file (styles partial-completion)))))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-common-palette-overrides '((bg-mode-line-active bg-lavender)))
  (modus-themes-load-theme 'modus-operandi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/notes")
	denote-prompts '(title)
	denote-sort-dired-extra-prompts nil
	denote-file-type nil))

(defvar-keymap kl/denote-map
  "n" #'denote
  "d" #'denote-dired
  "/" #'denote-grep)

(defvar-keymap kl/leader-map
  "k" #'kill-buffer
  "f" #'project-find-file
  "b" #'switch-to-buffer
  "/" #'consult-ripgrep
  "n" kl/denote-map)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC") kl/leader-map))
(global-set-key (kbd "C-SPC") kl/leader-map)
