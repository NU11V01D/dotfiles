;;; init.el
;;; Package Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Options
(global-display-line-numbers-mode 1)
;; Basic
(setq scroll-conservatively 101)

;;; Keymaps


;;; Packages
(use-package kanagawa-themes
  :ensure t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one))
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 2))

;;; Custom Functions
(defun my/toggle-relative-numbers ()
  "Toggles between relative and absolute line numbering."
  (interactive)
  (if (eq display-line-numbers-type 'relative)
      (setq display-line-numbers-type t) ;; Absolute line numbers
    (setq display-line-numbers-type 'relative))) ;; Relative line numbers otherwise
