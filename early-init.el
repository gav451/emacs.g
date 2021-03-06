;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lib/dash" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/packed" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/auto-compile" user-emacs-directory))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)
;;; early-init.el ends here
