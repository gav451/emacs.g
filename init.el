;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0.001)
  (setq use-package-verbose t))

(use-package subr-x
  :config
  (put 'if-let   'byte-obsolete-info nil)
  (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :demand t
  :commands
  auto-compile-on-load-mode
  auto-compile-on-save-mode
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package no-littering)

(use-package epkg
  :defer t
  :custom
  (epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands
  server-mode
  server-running-p
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(use-package ace-link
  :after avy
  :commands
  ace-link-setup-default
  :init
  (ace-link-setup-default))

(use-package ace-window
  :custom
  (aw-ignored-buffers '("*Calc Trail*"))
  (aw-leading-char-style 'path)
  (aw-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-reverse-frame-list t)
  :commands
  ace-window-display-mode
  :bind* ("C-x o" . ace-window)         ; was `other-window'.
  :after avy
  :init
  ;; C-h i: Elisp -> Display -> Faces -> Defining Faces.
  (face-spec-set 'aw-leading-char-face
                 '((((class color) (background dark))
                    :background "gold" :foreground "purple3" :height 2.0)
                   (((class color) (background light))
                    :background "purple3" :foreground "gold" :height 2.0)
                   (t :foreground "gray100" :underline nil))
                 'face-defface-spec)
  (ace-window-display-mode))

(use-package auctex
  ;; AuCTeX is better than the built in tex mode; let's use it.
  :load tex-site
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-auto-local ".auctex-auto-local")
  (TeX-auto-save t)
  (TeX-clean-confirm nil)
  (TeX-electric-escape t)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (TeX-parse-self t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  (reftex-plug-into-AUCTeX t)
  ;; (TeX-data-directory (expand-file-name "lib/auctex" user-emacs-directory))
  ;; (TeX-lisp-directory (expand-file-name "lib/auctex" user-emacs-directory))
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :config
  (use-package latex
    :commands
    TeX-latex-mode
    :hook
    (LaTeX-mode . LaTeX-math-mode)))

(use-package avy
  :custom
  (avy-all-windows t)
  :commands
  avy-setup-default
  :bind*
  ("C-:" . avy-goto-word-1)
  ("C-;" . avy-goto-char)
  :init
  (avy-setup-default))

(use-package company
  :preface
  (defun my-company-complete-number ()
    "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))
  (defun my-company-insert-abort ()
    (interactive)
    (company-abort)
    (self-insert-command 1))
  :custom
  (company-show-numbers t)
  :commands
  company-abort
  company-complete-number
  :bind (:map company-active-map
              ("C-i" . yas-expand-from-trigger-key)
              ("C-j" . company-complete-selection)
              ("C-k" . company-complete-common)
              (" " . my-company-insert-abort)
              ("0" . my-company-complete-number)
              ("1" . my-company-complete-number)
              ("2" . my-company-complete-number)
              ("3" . my-company-complete-number)
              ("4" . my-company-complete-number)
              ("5" . my-company-complete-number)
              ("6" . my-company-complete-number)
              ("7" . my-company-complete-number)
              ("8" . my-company-complete-number)
              ("9" . my-company-complete-number))
  :hook
  ((LaTeX-mode emacs-lisp-mode org-mode) . company-mode))

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (concat
                                    ;; file names beginning with # or .
                                    "\\(?:\\`[#\\.]\\)"
                                    ;; file names ending with # or ~
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"))
  (counsel-grep-swiper-limit (lsh 1 20))
  :bind
  ("C-r" . counsel-grep-or-swiper) ;; Was `isearch-backward'.
  ("C-s" . counsel-grep-or-swiper) ;; Was `isearch-forward'.
  :bind*
  ("C-h S" . counsel-info-lookup-symbol) ;; Was `info-lookup-symbol'.
  ("C-h f" . counsel-describe-function)  ;; Was `describe-function'.
  ("C-h v" . counsel-describe-variable)  ;; Was `describe-variable'.
  ("C-x C-f" . counsel-find-file)        ;; Was `find-file'.
  ("M-x" . counsel-M-x)                  ;; Was `execute-extended-command'.
  ("M-y" . counsel-yank-pop)             ;; Was `yank-pop'.
  ("C-c C-f" . counsel-recentf)
  ("C-c C-g" . counsel-rg)
  ("C-c u" . counsel-unicode-char))

(use-package dash
  :commands
  dash-enable-font-lock
  :config (dash-enable-font-lock))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  :commands
  global-diff-hl-mode
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :custom
  (dired-listing-switches "-alh"))

(use-package elec-pair
  :commands
  electric-pair-mode
  :config
  (electric-pair-mode))

(use-package epa
  :defer t
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (when (eq system-type 'darwin)
    (setq epg-gpg-program "gpg2")))

(use-package help
  :defer t
  :commands
  temp-buffer-resize-mode
  :config (temp-buffer-resize-mode))

(use-package ivy
  :demand t
  :custom
  (ivy-case-fold-search-default 'auto)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-use-ignore-default t)
  (ivy-use-virtual-buffers t)
  :bind* ("C-c C-r" . ivy-resume)
  :commands
  ivy-completing-read
  ivy-mode
  ivy-read
  :config (ivy-mode)
  :delight ivy-mode " 𝝓")

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package macrostep
  :bind*
  ("C-c e" . macrostep-expand)
  :config (use-package use-package))

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :hook
  (magit-add-section . magit-status-sections-hook)
  (magit-add-section . magit-insert-modules)
  (magit-add-section . magit-insert-stashes)
  (magit-add-section . append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package org
  :defer t
  :bind (("C-c l"   . org-store-link)
         ("C-c C-l" . org-insert-link-global))
  :commands
  org-link-set-parameters)

(use-package org-ref
  :after org
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path "~/VCS/research/papers")
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org")
  (org-ref-bibliography-notes "~/VCS/research/notes/notes.org")
  (org-ref-completion-liberary 'org-ref-ivy-cite)
  (org-ref-default-bibliography '("~/tmpfs/refs.bib" "~/VCS/research/refs.bib"))
  (org-ref-pdf-directory "~/VCS/research/papers"))

(use-package org-ref-glossary
  :after org-ref
  :commands
  or-follow-glossary)

(use-package org-ref-utils
  :after org-ref-glossary
  :functions
  org-ref-link-set-parameters
  :config
  (org-ref-link-set-parameters "ac*"
    :follow #'or-follow-glossary
    :face 'org-ref-glossary-face
    :help-echo 'or-glossary-tooltip
    :export (lambda (path _ format)
              (cond
               ((eq format 'latex)
                (format "\\gls*{%s}" path))
               (t
                (format "%s" path))))))

(use-package paren
  :commands
  show-paren-mode
  :config (show-paren-mode))

(use-package pdf-tools
  :custom
  (pdf-annot-activate-created-annotations t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-imagemagick t)
  :commands
  pdf-tools-install
  :magic
  ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t)
  (bind-keys :map pdf-view-mode-map
             ("C-r" . isearch-backward)
             ("C-s" . isearch-forward)))

(use-package prog-mode
  :preface
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  :hook
  (prog-mode . indicate-buffer-boundaries-left)
  :commands
  global-prettify-symbols-mode
  :config
  (global-prettify-symbols-mode))

(use-package python
  :custom
  (python-shell-interpreter-args "-E -i")
  :interpreter ("python" . python-mode)
  :mode ("\\.pyw?\\'" . python-mode)
  :init
  (use-package pyvenv
    :commands
    pyvenv-activate
    :config
    (pyvenv-activate "~/3.6"))
  :config
  (use-package elpy
    :custom
    (elpy-rpc-ignored-buffer-size (lsh 1 18))
    (elpy-modules '(elpy-module-sane-defaults
                    elpy-module-company
                    elpy-module-eldoc
                    elpy-module-flymake
                    elpy-module-pyvenv))
    (elpy-company-post-completion-function
     #'elpy-company-post-complete-parens)
    :commands
    elpy-company-post-complete-parens
    elpy-enable
    elpy-rpc
    elpy-rpc--buffer-contents
    :init
    (elpy-enable)
    (defcustom elpy-no-get-completions-rx
      "-?\\([0-9]+\\.?[0-9]*\\|0[Bb][01]+\\|0[Oo][0-8]+\\|0[Xx][0-9A-Fa-f]+\\)"
      "Let `elpy-rpc-get-completions' skip text matching this regexp.
Sometimes the jedi backend returns completions that confuse elpy, e.g.
for numbers.  Extend the regexp in case you find other similar cases
and file a bug report."
      :type 'string
      :group 'elpy)

    (defun elpy-rpc-get-completions (&optional success error)
      "Call the get_completions API function.

Returns a list of possible completions for the Python symbol at
point."
      (when (and (< (buffer-size) elpy-rpc-ignored-buffer-size)
                 (not (thing-at-point-looking-at elpy-no-get-completions-rx 32)))
        (elpy-rpc "get_completions"
                  (list buffer-file-name
                        (elpy-rpc--buffer-contents)
                        (- (point)
                           (point-min)))
                  success error)))))

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :commands
  savehist-mode
  :config (savehist-mode))

(use-package saveplace
  :commands
  save-place-mode
  :config (save-place-mode))

(use-package simple
  :commands
  column-number-mode
  :config (column-number-mode))

(use-package swiper
  :custom
  (swiper-action-recenter t)
  :bind (("C-s" . swiper)))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(use-package yasnippet
  :preface
  (defun yas-ivy-prompt (prompt choices &optional display-fn)
    (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
  :custom
  (yas-prompt-functions
   '(yas-ivy-prompt yas-completing-prompt))
  (yas-snippet-dirs
   (list (expand-file-name "yasnippet/snippets" no-littering-etc-directory)))
  :commands
  yas-expand-from-trigger-key
  yas-global-mode
  yas-next-field-or-maybe-expand
  yas-completing-prompt
  :demand t
  ;; I fail to use alternative keys in yas-keymap and yas-minor-mode-map as explained in
  ;; https://github.com/capitaomorte/yasnippet/blob/master/doc/faq.org.
  ;; However, everything works fine, sofar.
  :config
  (yas-global-mode 1)
  (delight 'yas-minor-mode " ✀"))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
