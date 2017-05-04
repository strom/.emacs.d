;;; package --- summary

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
(use-package adaptive-wrap)

(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-tern
    :ensure tern
    :init (add-to-list 'company-backends 'company-tern)))

;; Changed lines in gutter
(use-package diff-hl
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t))

(use-package dumb-jump
  :ensure hydra
  :bind (("M-g" . hydra-dumb-jump/body))
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
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

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
  :ensure hydra
  ;; Don't show useless minor mode in status bar.
  :diminish projectile-mode
  :config
  (projectile-mode)
  ;; Load extension only after projectile itself is loaded
  (use-package counsel-projectile
    :bind (("s-t" . counsel-projectile-find-file)
           ("s-p" . hydra-projectile/body)
           ("s-F" . counsel-projectile-ag))
    :config (counsel-projectile-on)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode)

(use-package tern
  :diminish tern-mode
  :init
  (add-hook 'web-mode-hook 'tern-mode)
  :config
  ;; Don't generate port files
  (add-to-list 'tern-command "--no-port-file" 'append))

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; Language Modes
(use-package feature-mode
  :mode (("\\.feature\\'" . feature-mode)))

(use-package jdee)

(use-package json-mode)

(use-package ruby-mode)

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

;; Some personal bindings
;; s-*, \s - Command
;; S-* - Shift
;; C-* - Control
;; M-* - Meta/Alt

;; macOS Navigation
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-<left>") 'back-to-indentation) ;; Move to the beginning of indented text
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(delete-selection-mode t)
 '(enable-recursive-minibuffers t)
 '(fill-column 120)
 '(global-auto-revert-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(load-prefer-newer t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(neo-show-updir-line nil nil nil "Since we use with projectile, we don't need to change root dirs.")
 '(neo-smart-open t)
 '(neo-theme (quote arrow))
 '(neo-vc-integration (quote (face)))
 '(neo-window-fixed-size nil)
 '(ns-right-command-modifier (quote left))
 '(package-selected-packages nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; init.el ends here
