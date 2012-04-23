(setq inhibit-splash-screen t)
(add-to-list 'load-path "~/.emacs.d")

(add-to-list 'load-path "~/.emacs.d/manual/evil")
(add-to-list 'load-path "~/.emacs.d/manual")
(add-to-list 'load-path "~/.emacs.d/el-get/evil")
(add-to-list 'load-path "~/.emacs.d/el-get/undo-tree")
(require 'evil)
(evil-mode 1)
;; (define-key evil-insert-state-map "C-" 'evil-normal-state) 
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
(define-key evil-normal-state-map ";"  'evil-ex)
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("blue" box))

(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

(menu-bar-mode 1)
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
(global-set-key [f5] 'redraw-display)

(global-set-key [f7] 'delete-trailing-whitespace)
(global-set-key [f8] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-8")))
(global-set-key [f9] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-9")))
(global-set-key [f10] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-10")))
(global-set-key [f11] (lambda ()
                       (interactive)
                       (set-frame-font "Monospace-11")))


;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") 
(unless (require 'el-get nil t) 
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
                (lambda (s) (end-of-buffer) (eval-print-last-sexp))))

;; C, C++
(setq c-default-style "k&r" 
      c-basic-offset 4)
(setq gdb-many-windows t)
              
;; asm
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(setq asm-comment-char ?#)

;; General editing
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/opt/google/chrome/google-chrome")

;; git
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)
(global-set-key "\C-c\g" 'magit-status)






