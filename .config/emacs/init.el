;;; init.el -*- lexical-binding: t; -*-
;; Inspired by `emacs-kick` by LionyxML.

;;; OPTIMIZATIONS
;; Increases the garbage collection threshold.
(setq gc-cons-threshold #x40000000)
;; Sets maximum output size for reading process output.
(setq read-process-output-max (* 1024 1024 4))

;;; PACKAGE SETUP
;; Makes it so you only need to ensure if you explicitly want to not load a package.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Adds MELPA as a package source
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; EMACS
;; TODO: Add comments until I figure out how to format ts.
(use-package emacs
  :ensure nil
  :custom
  (column-number-mode t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)

  :hook
  ;; Enable line numbers in programming modes.
  (prog-mode . display-line-numbers-mode)

  :config
  ;; Configure font settings based on the OS. Also, Neovim is objectively
  ;; better on terminal than Emacs. One of the killer features of Emacs
  ;; is literally having LaTeX rendering and inline images without doing
  ;; some weird fuckery with the terminal.
  (set-face-attribute 'default nil :family "CommitMonoVoid" :height 100)
  (when (eq system-type 'darwin)
    ;; (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "CommitMonoVoid" :height 130))

  ;; Save manual customizations to a separate file. Who thought cluttering
  ;; `init.el` was a good idea? Honestly.
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Changes Emacs vertical divisor for some reason.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (global-hl-line-mode -1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1)

  ;; Set the default coding system for files to UTF-8
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Hook to run something after Emacs initializes.
  (add-hook 'after-init-hook
	    (lambda ()
	      (message "Emacs has loaded.")
	      (with-current-buffer (get-buffer-create "*scratch*")
		(insert (format
			 ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
			 (emacs-init-time)
			 (number-to-string (length package-activated-list))))))))

;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

;;; DIRED
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '((".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq insert-directory-program gls)))))

;;; ISEARCH
(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")
  :bind (("C-s" . isearch-forward)
	 ("C-r" . isearch-backward)))

;;; VC
;; Built-in version control, tho it will be used alongside Magit.
(use-package vc
  :ensure nil
  :defer t
  :bind
  (("C-x v d" . vc-dir)
   ("C-x v =" . vc-diff)
   ("C-x v D" . vc-root-diff)
   ("C-x v v" . vc-next-action))
  :config
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

;;; SMERGE
;; Built-in merge conflict tool.
(use-package smerge-mode
  :ensure nil
  :defer t
  :bind (:map smerge-mode-map
	      ("C-c ^ u" . smerge-keep-upper)
	      ("C-c ^ l" . smerge-keep-lower)
	      ("C-c ^ n" . smerge-next)
	      ("C-c ^ p" . smerge-previous)))

;;; ELDOC
;; Built-in inline documentation for functions and variables in minibuffer.
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

;;; WHICH-KEY
;; Built-in keybinds helper. Oh, Emacs, you didn't have to!
(use-package which-key
  :ensure nil
  :defet t
  :hook
  (after-init . which-key-mode))

;;; ORG-MODE
;; Built-in markdown killer, ADHD med and absolute beast of a note system.
;; This config only loads it (for now). This is the thing that'll sell me
;; Emacs.
(use-package org
  :ensure nil
  :defet t)

;;; ============ EXTERNAL PACKAGES ============

;;; FLYCHECK
;; Flymake isn't that good apparently, so I'm using FlyCheck instead!
(use-package flycheck
  :init
  (global-flycheck-mode))

;;; COMPLETION-RELATED
;; VERTICO: Offers vertical completion options opposed to the default horizontal completion.
;; ORDERLESS: Enhances completion by allowing flexible pattern-matching.
;; MARGINALIA: Enhances completion by adding additional context to the completion candidates.
(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil)
  :config
  ;; Customize the display of the current candidate in the completion list.
  ;; This will prefix the current candidate with “» ” to make it stand out.
  ;; Copied from `emacs-kick` because I don't understand this part of the config.
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
                   "  ")
                 cand))))

(use-package orderless
  :defer t
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

;;; CONSULT
;; Enhances completion and also offers useful buffer navigation features.
(use-package consult
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;;; EMBARK
;; Provides a contextual action menu for Emacs.
(use-package embark
  :defer t)

;;; EMBARK-CONSULT
;; Bridge between Embark and Consult.
(use-package embark-consult
  :hook
  ;; Enable preview in Embark collect mode.
  (embark-collect-mode . consult-preview-at-point-mode))

;;; TREESITTER-AUTO
;; Simplifies the use of Tree-sitter in Emacs (provides auto-installation).
(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; MARKDOWN-MODE
;; Provides support for Markdown files in Emacs.
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown "multimarkdown"))

;;; NERD-ICONS-CORFU


;;; LSP


;;; LSP Additional Servers


;;; ELDOC-BOX


;;; DIFF-HL


;;; MAGIT


;;; INDENT-GUIDE


;;; ADD-NODE-MODULES-PATH


;;; UNDO TREE


;;; DOTENV


;;; DOOM MODELINE
;; The mode-line I tried to copy in Neovim using `lualine.nvim`.
(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  :hook
  (after-init . doom-modeline-mode))

;;; NERD ICONS
;; Provides Nerd Icons so I don't have to install extra fonts(?)
(use-package nerd-icons
  :defer t)

;;; NERD ICONS DIRED
;; Integrates said nerd icons to Dired.
(use-package nerd-icons-dired
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; NERD ICONS COMPLETION


;;; DOOM THEMES
(use-package doom-themes
  :config
  (load-theme 'doom-one t)

;;; init.el ends here
