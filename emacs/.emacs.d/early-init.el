;;; From Prot.
(defun kl/restore-theme (_f)
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))
(add-hook 'after-make-frame-functions #'kl/restore-theme)
(setq mode-line-format nil)
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(provide 'early-init)
;;; early-init.el ends here
