(setq inhibit-splash-screen t)
(setq viper-mode t)
(require 'viper)
(add-to-list 'load-path "~/.emacs.d")
(require 'vimpulse)

(require 'color-theme)
(color-theme-initialize)
(color-theme-gray30)

;(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))


(set-default-font "Monospace-10")
(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-c\c" 'compile)
(global-set-key "\C-x\r" 'align-regexp)
(global-set-key "\C-x\a" 'align-string)
(global-set-key "\C-c\C-t" 'ansi-term)
(global-set-key "\C-x\C-x" 'delete-frame)

(global-set-key [f8] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-8")))
(global-set-key [f9] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-9")))
(global-set-key [f10] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-10")))


;; General editing
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/opt/google/chrome/google-chrome")

;; git
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)
(global-set-key "\C-c\g" 'magit-status)







(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tuareg-begin-indent 4)
 '(tuareg-default-indent 4)
 '(tuareg-do-indent 4)
 '(tuareg-for-while-indent 4)
 '(tuareg-fun-indent 4)
 '(tuareg-function-indent 4)
 '(tuareg-if-then-else-indent 4)
 '(tuareg-indent-comments t)
 '(tuareg-let-indent 4)
 '(tuareg-match-indent 4)
 '(tuareg-method-indent 4)
 '(tuareg-rule-indent 4)
 '(tuareg-sig-struct-indent 4)
 '(tuareg-try-indent 4)
 '(tuareg-type-indent 4)
 '(tuareg-val-indent 4)
 '(tuareg-with-indent 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
