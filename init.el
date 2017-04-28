;;; package --- summary
;;; Commentary:
(require 'package)

;;; Code:
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; User customization
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
		'(javascript-jshint javascript-jscs javascript-gjslint javascript-standard)))
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.js[x]?\\'" . web-mode)
	 ("\\.json\\'" . web-mode))
  :init
  (defvar web-mode-content-types-alist)
  (setq web-mode-content-types-alist
	'(("jsx"  . "\\.js[x]?\\'")))
  :config
  ;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))
;; Typing replaces selection
(delete-selection-mode 1)
;; Show line and column numbers
(global-linum-mode t)
(setq column-number-mode t)
;; Highlight matching parens
(show-paren-mode 1)
;; Initial Window Size
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 128))
;; Theme
(load-theme 'zenburn t)

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global.
http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslit-executable"
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/setup-indent (n)
  "Indentation to N spaces."
  ;; I also set standard-indent with M-x customize-variable
  ;; java/c/c++
  ;; (setq c-basic-offset n)
  ;; web development
  ;; (setq coffee-tab-width n) ; coffeescript
  ;; (setq javascript-indent-level n) ; javascript-mode
  ;; (setq js-indent-level n) ; js-mode
  ;; (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  ;; (setq css-indent-offset n) ; css-mode
  )
(my/setup-indent 2)

;; Ivy
(ivy-mode 1)
(defvar ivy-use-virtual-buffers)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
;; Projectile Integration
(projectile-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (counsel-projectile ivy zenburn-theme web-mode projectile use-package flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
