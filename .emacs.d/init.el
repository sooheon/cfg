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
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(fringe-mode '(8 . 0)) 			; only have left fringe
(setq indicate-empty-lines t)

;;** Default behaviors
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default vc-follow-symlinks t)
(defun soo-prog-mode-init ()
  ;; increse fill-column to 100 while programming
  (setq-local comment-auto-fill-only-comments t)
  (setq-local fill-column 100))
(add-hook 'prog-mode-hook 'soo-prog-mode-init)
(setq auto-save-default nil)
(setq make-backup-file nil)
(save-place-mode 1)
(setq default-input-method "korean-hangul")
(setq sentence-end-double-space nil)
(column-number-mode 1)
(setq-default fill-column 80)
 
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

(defun soo--delete-window-or-kill-buffer ()
  "When there are multiple windows open, close active
one. Otherwise kill buffer."
  (interactive)
  (if (one-window-p)
      (kill-this-buffer)
    (delete-window)))

(use-package general
  :init
  (general-define-key
   "s-s" 'save-buffer
   "s-C" 'count-words
   "s-w" 'soo--delete-window-or-kill-buffer
   "s-o" 'find-file
   "s-v" 'yank
   "s-a" 'mark-whole-buffer)
  (general-define-key :keymaps 'minibuffer-local-map
		      [escape] 'minibuffer-keyboard-quit))

(use-package delight)

(use-package company
  :delight company-mode
  :hook (after-init . global-company-mode)
  :general (:keymaps 'company-active-map
		     "C-w" nil)
  :config
  (setq company-idle-delay 0.1
	company-show-numbers t
	company-minimum-prefix-length 2
	company-require-match nil
	company-tooltip-align-annotations t
	company-eclim-auto-save nil
	company-dabbrev-downcase nil)
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add
   'company-complete-selection
   :around #'jcs--company-complete-selection--advice-around))

;; Semantic config
(setq semanticdb-search-system-databases nil)

(use-package company-prescient
  :config
  (company-prescient-mode 1))

(use-package company-tabnine
  :disabled t
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package flx)

(use-package company-fuzzy
  :disabled t
  :after company
  :config
  (setq company-fuzzy-sorting-backend 'flx)
  (global-company-fuzzy-mode 1))

(use-package which-key
  :delight which-key-mode
  :init (which-key-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package evil
  :delight undo-tree-mode
  :general
  ("s-z" 'undo-tree-undo "s-Z" 'undo-tree-redo)
  (:states '(insert emacs)
	   "C-w" 'evil-delete-backward-word)
  (:states '(normal)
	   "C-u" 'evil-scroll-up
	   "M-." nil)
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-anzu
  :after evil
  :delight anzu-mode
  :config
  (global-anzu-mode t))

(use-package lispy
  :delight lispy-mode
  :hook ((emacs-lisp-mode
	  clojure-mode
	  clojurescript-mode
	  cider-repl-mode)
	 . lispy-mode)
  :diminish lispy-mode
  :general (:keymaps 'lispy-mode-map
		     "DEL" #'lispy-backward-delete
		     "[" #'lispy-brackets
		     "{" #'lispy-braces
		     "M-{" #'lispy-wrap-braces
		     "M-[" #'lispy-wrap-brackets
		     "M-(" #'lispy-wrap-round)
  (:keymaps 'global-map
	    "M-r" #'lispy-raise-sexp)
  :config
  (lispy-set-key-theme '(special parinfer c-digits))
  (setq lispy-eval-display-style 'overlay))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :delight lispyville-mode
  :general
  ([remap evil-normal-state] 'lispyville-normal-state)
  (:keymaps 'lispyville-mode-map
	    [M-down] 'lispyville-drag-forward
	    [M-up] 'lispyville-drag-backward)
  :init
  (setq lispyville-key-theme '(operators c-w escape slurp/barf-cp wrap escape)
	lispyville-barf-stay-with-closing t))

(use-package aggressive-indent
  :disabled t
  :diminish (global-aggressive-indent-mode aggressive-indent-mode)
  :delight (global-aggressive-indent-mode aggresive-indent-mode)
  :config
  (global-aggressive-indent-mode 1))

(use-package ivy
  :delight ivy-mode
  :general
  (:keymaps 'ivy-minibuffer-map
	    [escape] 'keyboard-escape-quit)
  ("s-b" 'ivy-switch-buffer)
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
		       "M-o" #'ivy-dispatching-done
		       "C-o" #'hydra-ivy/body)))


(use-package ivy-posframe
  :delight ivy-posframe-mode
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package prescient)
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :init (setq prescient-persist-mode t))

(use-package counsel
  :general
  ([remap apropos] #'counsel-apropos
   [remap describe-function] #'counsel-describe-function
   "s-e" #'counsel-buffer-or-recentf
   "s-i" #'counsel-semantic-or-imenu
   "s-j" #'soo--counsel-rg-or-grep
   "<C-tab>" #'counsel-switch-buffer
   "s-f" #'counsel-grep-or-swiper
   [remap switch-to-buffer] #'counsel-switch-buffer
   [remap mac-next-tab-or-toggle-tab-bar] #'counsel-recentf)
  :config
  (defun soo--counsel-rg-or-grep ()
    (interactive)
    (if (executable-find "rg")
	(call-interactively 'counsel-rg)
      (call-interactively 'counsel-grep))))

(use-package avy)

(use-package popwin
  :general
  (:keymaps 'help-mode-map [escape] 'quit-window)
  :config
  (popwin-mode 1))

(use-package worf
  :disabled t
  :hook (org-mode . worf-mode)
  :config (setq org-adapt-indentation nil))

(setq sooheon-org-dir "~/Documents/org/")

(use-package org
  :general
  (:keymaps 'org-mode-map
	    "<C-tab>" nil)
  :hook (org-mode . auto-fill-mode)
  :config
  (setq org-adapt-indentation nil))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
  :delight org-roam-mode
  :custom
  (org-roam-directory sooheon-org-dir)
  (org-roam-buffer-width 0.33)
  :bind (:map
	 org-roam-mode-map
	 (("C-c n l" . org-roam)
	  ("C-c n f" . org-roam-find-file)
	  ("C-c n g" . org-roam-show-graph))
	 :map
	 org-mode-map
	 (("C-c n i" . org-roam-insert))))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir sooheon-org-dir)
  (org-journal-date-format "%A, %d %B %Y"))

(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/"))

(use-package ace-window
  :general
  ("M-<tab>" 'ace-window))

(use-package projectile
  :general
  ("s-p" 'projectile
   "s-O" 'projectile-find-file
   "s-T" 'projectile-toggle-between-implementation-and-test))

(use-package magit
  :general
  ("s-9" 'magit-status
   "s-k" 'magit-status))

(use-package evil-magit)

(use-package magit-todos)

(use-package diff-hl
  :hook (((prog-mode conf-mode vc-dir-mode ledger-mode) . turn-on-diff-hl-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom (diff-hl-draw-borders . nil)
  :after hydra
  :config
  (defhydra hydra-diff-hl (:color red)
    "diff-hl"
    ("=" diff-hl-diff-goto-hunk "goto hunk")
    ("<RET>" diff-hl-diff-goto-hunk "goto hunk")
    ("u" diff-hl-revert-hunk "revert hunk")
    ("[" diff-hl-previous-hunk "prev hunk")
    ("p" diff-hl-previous-hunk "prev hunk")
    ("]" diff-hl-next-hunk "next hunk")
    ("n" diff-hl-next-hunk "next hunk")
    ("q" nil "cancel")))

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

(use-package verb
  :mode ("\\.verb\\'" . verb-mode))


;;** Langs
;;*** Lisps
(use-package clojure-mode
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :general
  (:keymaps 'cider-mode-map
	    "<s-return>" #'cider-eval-sexp-at-point
	    "M-." #'cider-find-var
	    "C-s-t" #'cider-test-run-project-tests)
  (:states '(normal) :keymaps 'cider-mode-map
	   "K" #'cider-doc)
  :hook
  (((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
   ((cider-mode cider-repl-mode) . eldoc-mode))
  :config
  (setq cider-prompt-save-file-on-load nil
	cider-repl-use-clojure-font-lock t
	cider-repl-pop-to-buffer-on-connect nil
	cider-clojure-cli-global-options "-A:test"
	cider-prompt-for-symbol nil
	cider-print-fn 'pprint))

(use-package clj-refactor
  :delight clj-refactor-mode
  :hook ((clojure-mode clojurescript-mode) . clj-refactor-mode)
  :config
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (add-to-list 'cljr-magic-require-namespaces '("edn" . "clojure.edn"))
  (setq cljr-warn-on-eval nil))

(use-package eldoc
  :delight eldoc-mode)

(use-package flycheck-clj-kondo
  :if (locate-file "clj-kondo" exec-path)
  :after (flycheck clojure-mode))

(use-package inf-clojure
  :config
  ;; using inf-clojure only for plank projects
  (setq inf-clojure-generic-cmd "plk"
	inf-clojure-tools-deps-cmd "plk"))

(use-package haskell-mode)

;;**** Markup langs
(use-package unfill
  :general ("M-q" 'toggle-fill-unfill))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)
	 ("/itsalltext/.*\\(gitlab\\|github\\).*\\.txt$" . gfm-mode)))

(use-package adoc-mode
  :mode ("\\.adoc\\'"))

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :delight elisp-slime-nav-mode
  :general
  (:states 'normal :keymaps 'emacs-lisp-mode-map
	   "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  (:keymaps 'emacs-lisp-mode-map
	    "M-." 'elisp-slime-nav-find-elisp-thing-at-point))

(use-package python
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
	     (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

;;*** Fish
(use-package company-shell)
(use-package fish-mode
  :mode "\\.fish\\'"
  :hook ((fish-mode shell-mode) . fish-mode-init)
  :config
  (defun fish-mode-init ()
    (setq-local company-backends '((company-fish-shell
				    company-shell
				    company-shell-env
				    company-files
				    company-dabbrev-code
				    company-yasnippet)))))

;;* IRC
(use-package circe
  :config
  (setq circe-network-options
	'(("Freenode"
	   :nick "sooheon"
	   :sasl-username "sooheon"
	   :sasl-password "buildallthosebridges"
	   :channels ("#dwarffortress" "#dfhack")))))

;;* Keybindings
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-command-modifier 'super
      mac-right-option-modifier 'meta)

(use-package yasnippet
  :delight yas-minor-mode)

;;* Themes
(use-package zenburn-theme :defer t)

(use-package doom-themes :defer t)

;; emacs server
(require 'server)
(unless (server-running-p) (server-start))
