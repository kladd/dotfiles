(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil :ensure t
	     :init
	     (progn
	       (use-package evil-leader
		 :ensure t
		 :init (progn
			 (global-evil-leader-mode)
			 (evil-leader/set-leader ",")))
	       (use-package evil-org
		 :ensure t
		 :init (add-hook'org-mode-hook 'evil-org-mode))
	       (evil-mode 1)))

(use-package org :ensure t
  :init
  (progn
    (setq org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "DONE")))
    (defun state-change-hook (state-plist)
      (if (and (string-equal (plist-get state-plist :to) "IN-PROGRESS")
	       (string-equal (plist-get state-plist :from) "TODO"))
	  (org-clock-in))
      (if (and (string-equal (plist-get state-plist :to) "TODO")
	       (string-equal (plist-get state-plist :from)
			     "IN-PROGRESS"))
	  (org-clock-out)))
    (add-hook 'org-trigger-hook 'state-change-hook)
    (setq org-log-done t)))

(use-package olivetti :ensure t
  :init
  (progn
    (define-globalized-minor-mode my-global-olivetti-mode olivetti-mode
      (lambda () (olivetti-mode)))
    (my-global-olivetti-mode 1)
    (olivetti-set-width 120)))

(use-package ido :init (ido-mode t))

(use-package doom-themes :ensure t
  :config
  (setq doom-themes-enable-bold nil
	doom-themes-enable-italic t)
  (load-theme 'doom-opera-light t)
  (doom-themes-org-config))

(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar t)
(set-face-attribute 'default nil :family "Monaco" :height 140)

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode 1)
(electric-pair-mode 1)

(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . "~/.backup")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-themes olivetti evil-org evil-leader evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
