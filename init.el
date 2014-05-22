(setq inhibit-splash-screen t)
;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
                (lambda (s) (end-of-buffer) (eval-print-last-sexp)))
  (require 'el-get))

(setq
 el-get-sources
 '(el-get
   evil
   key-chord
   magit
   markdown-mode
   ))
(el-get 'sync el-get-sources)


(require 'evil)
(evil-mode 1)

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)

(define-key evil-normal-state-map "t" 'find-tag)
(define-key evil-normal-state-map "T" 'pop-tag-mark)

(define-key evil-normal-state-map ";"  'evil-ex)
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("blue" box))

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)


(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)


(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))


(set-default-font "Monospace-8")
(add-to-list 'default-frame-alist '(font . "Monospace-8"))

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


;; Haskell
;(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; C, C++
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq gdb-many-windows t)
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

(defun includeguard-sym-name ()
  (concat
   (replace-regexp-in-string
    "[\\.-]" "_"
    (upcase (file-name-nondirectory (buffer-file-name))))
   "_INCLUDED"))

(defun includeguard-generate () (interactive)
  (let ((ig-sym (includeguard-sym-name)))
    (beginning-of-buffer)
    (insert "#ifndef " ig-sym "\n")
    (insert "#define " ig-sym "\n\n")
    (end-of-buffer)
    (insert "\n#endif /* " ig-sym " */\n")))

(global-set-key [f12] 'includeguard-generate)


;; asm
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(setq asm-comment-char ?#)

;; General editing
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/usr/bin/google-chrome")

;; git
(require 'magit)
(global-set-key "\C-c\g" 'magit-status)



;; closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (delete-frame)
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "C-x C-x") 'ask-before-closing)
