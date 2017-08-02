(setq inhibit-splash-screen t)

(require 'package)
(require 'cl)
;; Packages -------------------------------
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  '(evil
    markdown-mode
    color-theme
    haskell-mode
    xcscope
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Local installs
(add-to-list 'load-path "~/.emacs.d/from-web/key-chord-mode")


;; evil --------------------------------------------------
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

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "cv" 'comment-region)
(key-chord-define evil-visual-state-map "ui" 'uncomment-region)


;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)
(if(daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f)
                (with-selected-frame f
                  (if (window-system f)
                      (color-theme-dark-laptop))))))


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

;xcscope
(require 'xcscope)
(cscope-setup)


;; asm
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(setq asm-comment-char ?#)

;; General editing
(setq-default indent-tabs-mode nil)
(global-linum-mode 1)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/usr/bin/google-chrome")

;; git
;(require 'magit)
;(global-set-key "\C-c\g" 'magit-status)



;; closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (delete-frame)
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "C-x C-x") 'ask-before-closing)
