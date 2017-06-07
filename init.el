;; package --- summary

;;; Commentary:

;; My personal Emacs config

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslit-executable"
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (defvar flycheck-javascript-eslint-executable)
      (setq-local flycheck-javascript-eslint-executable eslint))))


;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ;; Always auto install packages

;; Installed packages
(use-package adaptive-wrap
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package autorevert
  ;; This package is part of core Emacs, so this import is solely to diminish the minor mode.
  :diminish auto-revert-mode)

(use-package company
  :diminish company-mode
  :config
  (use-package company-tern))

;; Launching Emacs from Applications needs this for ag.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Changed lines in gutter
(use-package diff-hl
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t))

(use-package dumb-jump
  :bind (("s-g" . hydra-dumb-jump/body))
  :config
  (defvar dumb-jump-selector)
  (setq dumb-jump-selector 'ivy))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("s-r" . er/expand-region)
         ("s-R" . er/contract-region)))

(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                '(javascript-jshint javascript-jscs javascript-gjslint javascript-standard))
  :config
  ;; Use eslint for web mode.
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package hydra
  ;; Load all hydras from load-path. This won't work in :config because we need it for other packages before this package lazy-loads.
  :init (use-package hydras
          :ensure nil ;; Local packages can't be ensured or it'll break config.
          :load-path "./lisp")
  :bind (("C-x k" . hydra-kill-buffer/body)))

(use-package ivy
  :init (ivy-mode)
  ;; Don't show useless minor mode in status bar.
  :diminish ivy-mode
  :bind (("\C-s" . swiper)
         ("s-f" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate))
  :config
  ;; Ivy
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

  ;; Ivy/Counsel support
  (use-package counsel-projectile
    ;; Bind in init so projectile can have counsel bindings even if this is lazy loaded.
    :init (counsel-projectile-on)
    :bind (("s-t" . counsel-projectile-find-file)
           ("s-p" . hydra-projectile/body)
           ("s-F" . counsel-projectile-ag))))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/unmark-next-like-this)))

(use-package move-lines
  ;; Move lines and regions with M-up/M-p and M-down/M-n
  :ensure nil ;; Local packages can't be ensured or it'll break config.
  :load-path "./lisp"
  :config (move-lines-binding))

(use-package neotree
  :bind (("s-\\" . neotree-project-dir))
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))))

(use-package projectile
  ;; Don't show useless minor mode in status bar.
  :diminish projectile-mode
  :config
  ;; Enable projectile key bindings and cache.
  (projectile-mode)

  ;; Ruby on Rails support
  (use-package projectile-rails
    ;; Don't defer the loading of projecile-rails with keybindings.
    :init (projectile-rails-global-mode)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :bind (("C-M-s" . hydra-smartparens/body))
  :init
  ;; Use recommended bindings
  (sp-use-smartparens-bindings))

(use-package tern
  :diminish tern-mode
  :init
  (add-hook 'web-mode-hook 'tern-mode)
  :config
  ;; Don't generate port files
  (add-to-list 'tern-command "--no-port-file" 'append))

(use-package wgrep)

(use-package which-key
  :diminish which-key-mode
  ;; Key bindng training wheels.
  :init (which-key-mode))

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; Language Modes
(use-package feature-mode
  :mode (("\\.feature\\'" . feature-mode)))

(use-package json-mode)

(use-package markdown-mode)

(use-package robe)

(use-package ruby-mode)

(use-package inf-ruby
  ;; This is so we can use pry or byebug
  :config
  (inf-ruby-switch-setup))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :init
  (defvar web-mode-content-types-alist)
  (setq web-mode-content-types-alist
        '(("jsx"  . "\\.js[x]?\\'")))
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

(use-package yaml-mode
  :mode (("\\.y[a]?ml\\'" . yaml-mode)))



;; Initial Window Size
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 128))

;; Remove extraneous whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Set visual-line-mode when coding
(add-hook 'prog-mode-hook #'visual-line-mode)

;; Some personal bindings
;; s-*, \s - Command
;; S-* - Shift
;; C-* - Control
;; M-* - Meta/Alt

;; Use ibuffer instead of buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; macOS Navigation
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-<left>") 'back-to-indentation) ;; Move to the beginning of indented text
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; Set Frame title
;; https://github.com/syl20bnr/spacemacs/issues/2139
(defun my/frame-title-format ()
  "Return frame title with current project name, where applicable."
  (let ((file buffer-file-name))
    (if 'file
        (concat (abbreviate-file-name file)
                (when (and (bound-and-true-p projectile-mode)
                           (projectile-project-p))
                  (format " [%s]" (projectile-project-name))))
      "%b")))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (my/frame-title-format)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 2)
 '(auto-save-file-name-transforms (\` ((".*" (\, temporary-file-directory) t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/.backups"))))
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-files company-nxml company-css company-eclim company-elisp
                   (company-tern company-capf company-keywords company-dabbrev-code company-abbrev company-etags)
                   company-semantic company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 2)
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(completion-styles (quote (initials partial-completion)))
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(delete-selection-mode t)
 '(enable-recursive-minibuffers t)
 '(fill-column 120)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(neo-auto-indent-point t)
 '(neo-autorefresh nil nil nil "This currently doesn't do what I want with projectile root customization.")
 '(neo-show-hidden-files t)
 '(neo-show-updir-line nil nil nil "Since we use with projectile, we don't need to change root dirs.")
 '(neo-smart-open t)
 '(neo-theme (quote arrow))
 '(neo-vc-integration (quote (face)))
 '(neo-window-fixed-size nil)
 '(ns-right-command-modifier (quote left))
 '(package-selected-packages
   (quote
    (exec-path-from-shell wgrep which-key zenburn-theme yaml-mode web-mode use-package smartparens robe rainbow-delimiters projectile-rails neotree multiple-cursors markdown-mode magit json-mode jdee hydra feature-mode expand-region dumb-jump diff-hl counsel-projectile company-tern adaptive-wrap)))
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(ruby-deep-indent-paren-style nil)
 '(ruby-insert-encoding-magic-comment nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow nil)))
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2)
 '(wgrep-auto-save-buffer t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray23"))))
 '(region ((t (:background "gray33"))) nil "Make selected region more obvious")
 '(web-mode-current-element-highlight-face ((t (:underline t)))))

(provide '.emacs)
;;; init.el ends here
