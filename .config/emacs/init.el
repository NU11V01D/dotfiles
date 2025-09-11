;;; init.el
;;; Package Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Options
;; Basic
(tool-bar-mode -1) ;; Disable default Emacs Toolbar.
(menu-bar-mode -1) ;; Disable default Emacs Menubar.
(delete-selection-mode t) ;; Something something deletes something.
(setq custom-file (make-temp-file "EmacsCustom")) ;; Make a temp-file to save the annoying custom files.
(setq inhibit-startup-screen t) ;; Disable default Emacs startup screen.
;; Editor
(setq scroll-conservatively 101)
(setq display-line-numbers t)
(setq display-line-numbers-type 'relative)

;;; Keymaps


;;; Packages
(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-wave t))
(use-package doom-themes
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 2))
