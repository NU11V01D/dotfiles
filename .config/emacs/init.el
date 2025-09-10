;;; init.el
;;; Package Setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Options
(setq inhibit-startup-screen t "Disable startup scree.")
(setq scroll-conservatively 101 "Smooth scrolling")
(setq display-line-numbers t "Show line numbers")
(setq display-line-numbers-type 'relative "Show relative line numbers")
(setq-default cursor-line t "Activates the cursorline")

;;; Keymaps


;;; Packages


