(setq package-enable-at-startup nil
      inhibit-startup-message t
      frame-resize-pixelwise t
      package-native-compile t
      cursor-in-non-selected-windows nil
      confirm-nonexistent-file-or-buffer nil
      completion-styles '(basic substring)
      tab-width 8
      vc-follow-symlinks t)

(setq gc-cons-threshold (* 1024 1024 1024))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq x-underline-at-descent-line t
      window-divider-default-right-width 12
      window-divider-default-paces 'right-only)
(window-divider-mode 1)

;; Emacs 29 scrolling
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 10.0
      scroll-conservatively 1000
      scroll-margin 4)

;; Set a backup directory I'll never look at.
(setq backup-directory-alist `(("." . ,(expand-file-name ".backups"
							 user-emacs-directory))))
;; Write custom-* stuff not in this file.
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(set-face-attribute 'default nil
		    :font "Berkeley Mono"
		    :height 110)

(setq modus-themes-mode-line '(accented borderless (padding 4)))
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)
(setq modus-themes-operandi-color-overrides
      '((bg-active-accent . "#f8f8f8")
	(fg-window-divider-inner . "#f8f8f8")
	(fg-window-divider-outer . "#f8f8f8")))
(load-theme 'modus-vivendi t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
      use-package-always-ensure t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package-ensure)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-define-key 'motion org-mode-map (kbd "C-\[") 'org-open-at-point))

(use-package org
  :init (setq org-hide-emphasis-markers t)
  :hook (org-mode . kl/org-mode-hook))

(use-package org-roam
  :custom
  (org-roam-directory "~/log")
  (org-roam-completion-everywhere t)
  :bind (("C-c l" . org-roam-buffer-toggle)
         ("C-c f" . org-roam-node-find)
         ("C-c i" . kl/org-roam-insert-imm)
         :map org-mode-map
         ("C-c p" . completion-at-point))
  :bind-keymap ("C-c d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("c" . "src c"))

(use-package visual-fill-column
  :hook (org-mode . (lambda () (kl/visual-fill 80))))

;; Auto completion
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  :init (global-corfu-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

;; Syntax
(setq treesit-language-source-alist
      '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json"))
	(make . ("https://github.com/alemuller/tree-sitter-make"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))

;; Rust IDE
(use-package flycheck)
(setq rust-format-on-save t
      rust-rustfmt-switches '("+nightly --all"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'before-save-hook 'eglot-format)

(defun kl/install-langs ()
  (let ((langs (mapcar 'car treesit-language-source-alist)))
    (dolist (lang langs)
      (treesit-install-language-grammar lang))))

(defun kl/visual-fill (width)
  (setq visual-fill-column-width width
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun kl/org-mode-hook ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c) (if (char-equal c ?<) t
                             (,electric-pair-inhibit-predicate c))))
  (org-indent-mode)
  (visual-line-mode 1)
  (display-line-numbers-mode 0))

(defun kl/org-roam-insert-imm (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args))) 

(defun kl/set-emacs-frames (variant)
  "Sets GTK frame variant"
  (dolist (frame (frame-list))
    (let* ((window-id (frame-parameter frame 'outer-window-id))
	   (id (string-to-number window-id))
	   (cmd (format "xprop -id 0x%x -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\""
			id variant)))
      (call-process-shell-command cmd))))
(kl/set-emacs-frames "dark")
