(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
     (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;* Appearance
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
 
;;** Fonts
(set-face-attribute 'default nil :family "Input Mono Narrow")
(set-face-attribute 'default nil :height 140)
(set-fontset-font t 'hangul (font-spec :name "NanumGothic"))

;;* Straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;** Packages
(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file
	(no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :init
  (general-define-key
   "s-s" 'save-buffer
   "s-C" 'count-words
   "s-w" 'kill-current-buffer)
  (general-define-key
   :keymaps 'minibuffer-local-map
   [escape] 'minibuffer-keyboard-quit))

(use-package which-key
  :init (which-key-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package evil
  :general
  ("s-z" 'undo-tree-undo "s-Z" 'undo-tree-redo)
  (:states
   '(insert emacs)
   "C-w" 'evil-delete-backward-word)
  (:states
   '(normal)
   "M-." nil)
  :init
  (setq evil-disable-insert-state-bindings t)
  (evil-mode t))

(use-package lispy
  :hook ((emacs-lisp-mode
	  clojure-mode
	  clojurescript-mode
	  cider-repl-mode)
	 . lispy-mode)
  :diminish lispy-mode
  :general (:keymaps 'lispy-mode-map
		     "DEL" 'lispy-backward-delete
		     "[" 'lispy-brackets
		     "{" 'lispy-braces
		     "M-{" 'lispy-wrap-braces
		     "M-[" 'lispy-wrap-brackets
		     "M-(" 'lispy-wrap-round)
  :config
  (lispy-set-key-theme '(special parinfer c-digits))
  (setq lispy-eval-display-style 'overlay))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :diminish lispyville-mode
  :general
  ([remap evil-normal-state] 'lispyville-normal-state)
  (:keymaps 'lispyville-mode-map
	    [M-down] 'lispyville-drag-forward
	    [M-up] 'lispyville-drag-backward)
  :init
  (setq lispyville-key-theme '(operators c-w escape slurp/barf-cp wrap escape)
	lispyville-barf-stay-with-closing t))

(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers nil
	ivy-virtual-abbreviate 'full
	enable-recursive-minibuffers t
	search-default-mode #'char-fold-to-regexp
	ivy-height 12
	ivy-wrap t
	projectile-completion-system 'ivy )

  (use-package ivy-hydra
    :commands (ivy-dispatching-done ivy--matcher-desc ivy-hydra/body)
    :general (:keymaps 'ivy-minibuffer-map
		       "C-o" #'ivy-dispatching-done
		       "M-o" #'hydra-ivy/bod)))

(use-package prescient)
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :init (setq prescient-persist-mode t))
(use-package company-prescient)

(use-package counsel
  :general
  ([remap apropos] #'counsel-apropos
   [remap describe-function] #'counsel-describe-function
   "s-e" 'counsel-recentf
   "s-j" 'counsel-rg
   "s-o" 'find-file
   "<C-tab>" 'counsel-switch-buffer
   [remap switch-to-buffer] #'counsel-switch-buffer
   [remap mac-next-tab-or-toggle-tab-bar] 'counsel-recentf))

(use-package ace-window
  :general
  ("s-;" 'ace-window))

(use-package magit
  :general
  ("s-9" 'magit-status
   "s-k" 'magit-status)
  :config (setq magit-status-buffer-switch-function))

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :config
  (setq flycheck-popup-tip-error-prefix "x "))

(use-package flycheck-posframe)

(use-package golden-ratio
  :general (:states
	    '(normal)
	    :prefix "SPC"
	    "tg" #'golden-ratio))


;;** Langs
(use-package clojure-mode)

(use-package clj-refactor
  :hook ((clojure-mode clojurescript-mode) . clj-refactor-mode)
  :config
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package cider
  :general (:keymaps 'cider-mode-map
		     "<s-return>" #'cider-eval-sexp-at-point)
  :config
  (setq cider-promt-save-file-on-load nil
	cider-repl-use-clojure-font-lock t
	cider-repl-pop-to-buffer-on-connect nil))

(use-package inf-clojure
  :config
  ;; using inf-clojure only for plank projects
  (setq inf-clojure-generic-cmd "plk"
	inf-clojure-tools-deps-cmd "plk"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :general
  (:states
   'normal :keymaps 'emacs-lisp-mode-map
   "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  (:keymaps
   'emacs-lisp-mode-map
   "M-." 'elisp-slime-nav-find-elisp-thing-at-point))

(use-package python
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
	     (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

;;* Keybindings
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil)
