;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn                                  ; startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq                                 ; `C-source'
   user-init-file (or load-file-name buffer-file-name)
   user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when t                        ; fast immediate and deferred loading
    ;; https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/init.el
    (defvar file-name-handler-alist-backup file-name-handler-alist)
    (setq file-name-handler-alist nil)
    (setq gc-cons-threshold (* 16 4096 4096))
    (let ((enough (if (getenv "EXWM") 15 5)))
      (add-hook 'after-init-hook
                (lambda ()
                  (run-with-idle-timer
                   enough nil
                   (lambda ()
                     (setq file-name-handler-alist
                           (cl-union file-name-handler-alist-backup
                                     file-name-handler-alist))
                     (setq gc-cons-threshold
                           (* 5 (car (get 'gc-cons-threshold 'standard-value))))
                     (message "[after-init] reset fast to normal speed")
                     (garbage-collect))))
                t)))
  (setq-default                         ; `C-source'
   cursor-type 'box
   indent-tabs-mode nil
   tab-width 8)
  (setq                                 ; `C-source'
   garbage-collection-messages t
   maximum-scroll-margin 0.25
   scroll-conservatively 0
   scroll-margin 0
   scroll-preserve-screen-position t)
  (setq                                 ; `startup'
   inhibit-startup-buffer-menu t
   inhibit-startup-screen t
   initial-buffer-choice t
   initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  ;; Darwin
  (when (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier nil))
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
  (when (boundp 'ns-right-command-modifier)
    (setq ns-right-command-modifier 'super))
  (when (eq window-system 'ns)
    (add-to-list 'initial-frame-alist '(height . 51))
    (add-to-list 'initial-frame-alist '(width . 180))))

(progn                                  ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn                                  ; `use-package' and `delight'
  ;; Must be set before loading use-package.
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'delight)
  (setq use-package-always-defer t)
  (setq use-package-minimum-reported-time 0.001)
  (setq use-package-verbose t))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer               nil)
  (auto-compile-mode-line-counter            t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest   t)
  (auto-compile-update-autoloads             t)
  :hook
  ((auto-compile-inhibit-compile
    ) . auto-compile-inhibit-compile-detached-git-head)
  )

(use-package no-littering
  :commands (no-littering-expand-etc-file-name
             no-littering-expand-var-file-name)
  :demand t)

(use-package custom
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :commands (custom-declare-variable)
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :when window-system
  :unless (or noninteractive (daemonp))
  :hook
  (after-init . server-start))

(progn                                  ; startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;; AUCTeX & friends
(use-package bibtex
  :after tex-site
  :mode ((rx (seq ".bib" eos)) . bibtex-mode)
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path '("~/VCS/research/papers"))
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org")
  (bibtex-user-optional-fields
   '(("abstract")
     ("doi" "Digital Object Identifier")
     ("url" "Universal Ressource Locator"))))

(use-package latex
  :after tex-site
  :mode ((rx (seq ".tex" eos)) . TeX-latex-mode)
  :custom
  (LaTeX-electric-left-right-brace t)
  :hook
  ((LaTeX-mode) . LaTeX-math-mode)
  :commands (LaTeX-narrow-to-environment))

(use-package reftex
  :after tex-site
  :hook
  ((LaTeX-mode) . reftex-mode)
  :delight (reftex-mode " 📑"))

(use-package reftex-vars
  :custom
  (reftex-default-bibliography "~/VCS/research/refs.bib")
  (reftex-plug-into-AUCTeX t))

(use-package tex
  :after tex-site
  :custom
  (TeX-auto-local ".auctex-auto-local")
  (TeX-auto-save t)
  (TeX-electric-escape nil)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (TeX-engine 'default)
  (TeX-master t)
  (TeX-parse-self t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  :hook
  ((LaTeX-mode) . TeX-PDF-mode)
  :commands (TeX-doc))

(use-package tex-buf
  :after tex-site
  :hook
  ((TeX-after-compilation-finished-functions
    ) . TeX-revert-document-buffer))

(use-package tex-site
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  ;; https://gitlab.com/jabranham/emacs
  ;; Use AUCTeX, since it is better than the built in tex mode.
  ;; Tweak .gitmodules to make the git repository resemble the elpa package.
  ;; Do not require auctex, since auctex.el provides no feature 'auctex'.
  :defer 2)

;; alphabetical order
(use-package ace-window
  :disabled
  :custom
  (aw-ignored-buffers '("*Calc Trail*"))
  (aw-leading-char-style 'path)
  (aw-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-reverse-frame-list t)
  :custom-face
  ;; C-h i: Elisp -> Display -> Faces -> Defining Faces.
  (aw-leading-char-face ((((class color) (background dark))
                          :background "gold" :foreground "purple3" :height 2.0)
                         (((class color) (background light))
                          :background "purple3" :foreground "gold" :height 2.0)
                         (t :foreground "gray100" :underline nil)))
  :commands (ace-window-display-mode)
  :bind (:map global-map
              ("M-o" . ace-window))
  :after avy
  :init
  (ace-window-display-mode))

(use-package alert
  :custom
  (alert-default-style 'libnotify))

(use-package autorevert
  :custom
  (auto-revert-mode-text " ⏎")
  :hook
  ((dired-mode) . auto-revert-mode))

(use-package avy
  :disabled
  :custom
  (avy-all-windows t)
  :commands (avy-setup-default)
  :bind* (("C-:" . avy-goto-word-1))
  :init
  (avy-setup-default))

(use-package browse-url
  :preface
  (defun dict-en (word)
    "Look up a word in the dictionary at 'https://thefreedictionary.com'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'https://www.thefreedictionary.com' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "https://www.thefreedictionary.com/" word)))

  (defun dict-nl (word)
    "Look up a word in the dictionary at 'https://www.woorden.org'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'https://www.woorden.org' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "https://www.woorden.org/woord/" word)))

  (defun dict-fr (word)
    "Look up a word in the dictionary at 'http://www.cnrtl.fr'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search http://www.cnrtl.fr for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://www.cnrtl.fr/definition/academie9/" word)))

  (defun webster (word)
    "Look up a word in the dictionary at 'https://webster-dictionary.org'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'https://webster-dictionary.org' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "https://webster-dictionary.org/definition/" word)))

  ;; https://github.com/chubin/wttr.in
  (defun weather (place)
    "Get a weather report."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Get weather from http://wttr.in/ (:help for help) for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://wttr.in/" place)))
  :custom
  (browse-url-browser-function
   '((".*github.*" . browse-url-generic)
     (".*gitlab.*" . browse-url-generic)
     (".*google.*" . browse-url-generic)
     (".*openstreetmap.org" . browse-url-generic)
     (".*readthedocs.org" . browser-url-generic)
     (".*reddit.com" . browse-url-generic)
     (".*wikipedia.*" . browse-url-generic)
     (".*youtube.*" . browse-url-generic)
     ("." . eww-browse-url)))
  (browse-url-generic-program (or (when (eq system-type 'darwin)
                                    "open")
                                  (executable-find "firefox")
                                  (executable-find "qutebrowser")))
  :commands (browse-url
             browse-url-generic))

(use-package deadgrep
  :bind ((:map global-map
               ("M-g d" . deadgrep))
         (:map deadgrep-mode-map
               ("C-c C-w" . deadgrep-edit-mode))))

(use-package company
  ;; https://emacs.stackexchange.com/questions/9835/how-can-i-prevent-company-mode-completing-numbers
  :custom
  (company-show-numbers t)
  :hook
  ((LaTeX-mode
    emacs-lisp-mode
    org-mode
    sly-mode
    sly-mrepl-mode) . company-mode)
  :delight (company-mode " 👫"))

(use-package company-prescient
  :hook
  ((company-mode) . company-prescient-mode))

(use-package company-yasnippet
  :bind ((:map global-map ("C-c y" . company-yasnippet))))

(use-package compile
  :delight (compilation-in-progress " 👷"))

(use-package counsel
  :disabled
  :custom
  (counsel-grep-swiper-limit (lsh 1 20))
  (counsel-locate-cmd (cond ((eq system-type 'darwin)
                             'counsel-locate-cmd-mdfind)
                            (t
                             'counsel-locate-cmd-default)))
  :bind ((:map global-map
               ;; dired-mode-map and isearch-mode-map shadow "M-s".
               ("M-s" . counsel-grep-or-swiper))))

(use-package counsel
  :disabled
  ;; https://www.reddit.com/r/emacs/comments/baby94/some_ivy_hacks/
  :preface
  (defun counsel-helpful-keymap-describe ()
    "select keymap with ivy, display help with helpful"
    (interactive)
    (ivy-read "describe keymap: "
              (let (cands)
                (mapatoms
                 (lambda (x)
                   (and (boundp x) (keymapp (symbol-value x))
                        (push (symbol-name x) cands))))
                cands)
              :require-match t
              :history 'counsel-describe-keymap-history
              :preselect (ivy-thing-at-point)
              :keymap counsel-describe-map
              :sort t
              :action (lambda (map-name)
                        (helpful-variable (intern map-name)))
              :caller 'counsel-helpful-keymap-describe))
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   (rx (or
        ;; file names beginning with # or .
        (seq bos (any "#."))
        ;; file names ending with # or ~
        (seq bos (+? nonl) (any "#~") eos)
        ;; file names ending with .elc or .pyc
        (group "." (group (or "el" "py") "c" eos)))))
  (counsel-grep-swiper-limit (lsh 1 20))
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-command-only)
  (counsel-locate-cmd (cond ((eq system-type 'darwin)
                             'counsel-locate-cmd-mdfind)
                            (t
                             'counsel-locate-cmd-default)))
  :commands (counsel-describe-face
             counsel-linux-app-format-function-command-only)
  :bind ((:map global-map
               ("C-r" . counsel-grep-or-swiper)
               ("C-s" . counsel-grep-or-swiper)
               ;; Avoid shadowing `eshell-forward-argument'.
               ("C-c C-f" . counsel-recentf)
               ;; Avoid shadowing `emms-playlist-mode-yank-pop'.
               ("M-y" . counsel-yank-pop))
         (:map help-map
               ("S" . counsel-info-lookup-symbol)
               ("f" . counsel-describe-function)
               ("v" . counsel-describe-variable)))
  :bind* (("C-x C-f" . counsel-find-file)
          ("M-x" . counsel-M-x)
          ("C-c C-g" . counsel-rg)
          ("C-c u" . counsel-unicode-char)))

(use-package cython-mode
  :mode ((rx (seq ".py" (any "xdi") eos)) . cython-mode))

(use-package dash
  :commands (dash-enable-font-lock)
  :config
  (dash-enable-font-lock))

(use-package diff-hl
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :custom
  (diff-hl-draw-borders nil)
  :hook
  ((dired-mode) . diff-hl-dired-mode)
  ((magit-post-refresh) . diff-hl-magit-post-refresh))

(use-package dired
  :preface
  (defun my-dired-eww-find-file ()
    "Visit dired file with eww."
    (interactive)
    (eww-open-file (dired-get-file-for-visit)))
  (defun my-dired-rsync (target)
    "Copy marked files with `rsync' to TARGET directory."
    (interactive
     (list (expand-file-name
            (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
    ;; Store all selected files into "files" list.
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          ;; the rsync command
          (rsync-command "rsync -av --progress "))
      ;; Add all marked files as arguments to the rsync command
      (dolist (file files)
        (setq rsync-command
              (concat rsync-command
                      (if (string-match "^/ssh:\\(.*\\)$" file)
                          (format " -e ssh %s" (match-string 1 file))
                        (shell-quote-argument file)) " ")))
      ;; Append the target.
      (setq rsync-command
            (concat rsync-command
                    (if (string-match "^/ssh:\\(.*\\)$" target)
                        (format " -e ssh %s" (match-string 1 target))
                      (shell-quote-argument target))))
      ;; Run the async shell command.
      (async-shell-command rsync-command)
      ;; Finally, switch to that window.
      (other-window 1)))
  :custom
  (dired-listing-switches "-aGlhv1 --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  :commands (dired-get-file-for-visit
             dired-get-marked-files)
  :config
  (bind-keys :map dired-mode-map
             ("E" . my-dired-eww-find-file)
             ("M-s y" . my-dired-rsync)))

(use-package dired-aux
  :after dired
  :commands (dired-dwim-target-directory))

(use-package dired-filter
  :after dired
  :custom
  (dired-filter-mark-prefix "\\")
  (dired-filter-prefix "/")
  (dired-filter-saved-filters
   '(("program files"
      (extension "el" "py"))
     ("org-mode files"
      (extension . "org"))
     ("text files"
      (extension . "txt"))
     ("latex files"
      (extension "bib" "tex"))
     ("document viewer files"
      (extension "djvu" "dvi" "epub" "pdf" "ps"))
     ("image files"
      (extension "gif" "jpeg" "jpg" "png" "tif" "tiff"))
     ("document editor files"
      (extension "doc" "docx" "odt" "ppt" "pptx" "xls" "xlsx"))
     ("multimedia files"
      (extension "avi" "mk4" "mkv" "mp4"))))
  (dired-filter-group-saved-groups
   '(("default"
      ("Directories" (directory))
      ("Program Files" "program files")
      ("Org-mode Files" "org-mode files")
      ("Text Files" "text files")
      ("LaTeX Files" "latex files")
      ("Document Viewer Files" "document viewer files")
      ("Image Files" "image files")
      ("Document Editor Files" "document editor files")
      ("Multimedia Files" "multimedia files"))))
  :bind ((:map dired-mode-map
               ("M-s g" . dired-filter-group-mode))))

(use-package dired-narrow
  :after dired
  :bind ((:map dired-mode-map
               ("M-s n" . dired-narrow))))

(use-package dired-subtree
  :after dired
  :bind ((:map dired-mode-map
               ("M-s i" . dired-subtree-insert)
               ("M-s r" . dired-subtree-remove))))

(use-package dired-x
  :after dired)

(use-package display-line-numbers
  :custom-face
  (line-number-current-line ((t (:inherit highlight))))
  :hook
  ((LaTeX-mode
    org-mode
    prog-mode) . display-line-numbers-mode))

(use-package easy-kill
  ;; https://emacsredux.com/blog/2018/11/09/an-easy-kill/
  ;; https://emacsredux.com/blog/2019/01/10/the-emacs-year-in-review/
  :bind ((:map global-map
               ("M-w" . easy-kill))))

(use-package eldoc
  :delight (eldoc-mode " 🛈"))

(use-package electric
  ;; https://github.com/davidshepherd7/dotfiles/blob/master/emacs/.emacs.d/lisp/ds-python.el
  :preface
  (defun my-enclosing-paren ()
    "Return the opening parenthesis of the enclosing parens, or
nil if not inside any parens."
    (let ((ppss (syntax-ppss)))
      (when (nth 1 ppss)
        (char-after (nth 1 ppss)))))
  (defun my-python-electric-newline ()
    (let ((paren (my-enclosing-paren)))
      (if (not (or (eq paren ?\{)
                   (eq paren ?\[)
                   (eq paren ?\()
                   (looking-back "\\blambda\\b.*" (point))))
          'after
        nil)))
  (defun on-python-mode-hook-electric-layout ()
    (make-local-variable 'electric-layout-rules)
    (add-to-list 'electric-layout-rules (cons ?: #'my-python-electric-newline))
    (electric-layout-mode))
  :commands (electric-layout-mode)
  :hook
  ((python-mode) . on-python-mode-hook-electric-layout))

(use-package electric-operator
  :hook
  ((python-mode) . electric-operator-mode)
  :delight (electric-operator-mode " ⨄⌤"))

(use-package elfeed
  ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  ;; https://gitlab.com/jabranham/emacs/blob/master/init.el
  :preface
  (defun elfeed+db-load+update ()
    "Enter elfeed, load the database, and update."
    (interactive)
    (elfeed)
    (elfeed-db-load)
    (elfeed-search-update :force)
    (elfeed-update))
  (defun elfeed-db-save+quit ()
    (interactive)
    (elfeed-db-save)
    (quit-window))
  :custom
  (elfeed-db-directory "~/SYNC/elfeed/db")
  (elfeed-feeds
   '(("http://emacshorrors.com/feed.atom" schneidermann)
     ("http://emacsninja.com/feed.atom" schneidermann)
     ("http://www.howardism.org/index.xml" howard)
     ("http://pragmaticemacs.com/feed/" maugham)
     ("http://sachachua.com/blog/category/emacs/feed" chua)
     ("https://act.eff.org/action.atom" eff)
     ("https://kitchingroup.cheme.cmu.edu/blog/feed" kitchin)
     ("https://ambrevar.xyz/atom.xml" neirhardt)
     ("https://feeds.feedburner.com/InterceptedWithJeremyScahill" intercepted)
     ("https://feeds.feedburner.com/TheMouseVsThePython" python)
     ("https://nullprogram.com/feed/" wellons)
     ("https://oremacs.com/atom.xml" krehel)
     ;; https://www.emacswiki.org/emacs/PlanetEmacsen
     ("https://planet.emacslife.com/atom.xml" emacsen)
     ("https://realpython.com/atom.xml" python)
     ("https://vxlabs.com/index.xml" vxlabs)
     ("https://www.aclu.org/taxonomy/feed-term/2152/feed" aclu)
     ("https://www.bof.nl/rss/" bof)
     ("https://www.democracynow.org/podcast-video.xml" dn)
     ("https://www.laquadrature.net/fr/rss.xml" lqdn)))
  (elfeed-enclosure-default-dir (expand-file-name "~/tmpfs/"))
  :bind* (("C-x w" . elfeed+db-load+update))
  :bind ((:map elfeed-search-mode-map
               ("?" . describe-mode)
               ("q" . elfeed-db-save+quit))
         (:map elfeed-show-mode-map
               ("?" . describe-mode)))
  :commands (elfeed
             elfeed-db-load
             elfeed-db-save
             elfeed-search-set-filter
             elfeed-search-toggle-all
             elfeed-search-update
             elfeed-update)
  :config
  (make-directory elfeed-db-directory t)
  (with-eval-after-load 'hydra
    (bind-key
     "f"
     (defhydra hydra-elfeed-filter ()
       ("A" (elfeed-search-set-filter "@48-months-ago") "All"
        :column "A-Z")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("S" (elfeed-search-set-filter "@12-months-ago +*") "Starred")
       ("U" (elfeed-search-set-filter "@12-months-ago +unread") "Unread")
       ("ab" (elfeed-search-set-filter "@48-months-ago +howard") "abrams"
        :column "a-c")
       ("ac" (elfeed-search-set-filter "@12-months-ago +aclu") "aclu")
       ("b" (elfeed-search-set-filter "@12-months-ago +bof") "bof" )
       ("c" (elfeed-search-set-filter "@48-months-ago +chua") "chua")
       ("d" (elfeed-search-set-filter "@12-months-ago +dn") "dn")
       ("ef" (elfeed-search-set-filter "@12-months-ago +eff") "eff"
        :column "e-k")
       ("em" (elfeed-search-set-filter "@12-months-ago +emacsen") "emacsen")
       ("i" (elfeed-search-set-filter "@12-months-ago +intercepted") "intercepted")
       ("ki" (elfeed-search-set-filter "@48-months-ago +kitchin") "kitchin")
       ("kr" (elfeed-search-set-filter "@48-months-ago +krehel") "krehel")
       ("l" (elfeed-search-set-filter "@12-months-ago +lqdn") "lqdn"
        :column "l-s")
       ("m" (elfeed-search-set-filter "@48-months-ago +maugham") "maugham")
       ("n" (elfeed-search-set-filter "@48-months-ago +neirhardt") "neirhardt")
       ("p" (elfeed-search-set-filter "@12-months-ago +python") "python")
       ("s" (elfeed-search-set-filter "@48-months-ago +schneidermann") "schneidermann")
       ("v" (elfeed-search-set-filter "@48-months-ago +vxlabs") "vxlabs"
        :column "v-w")
       ("w" (elfeed-search-set-filter "@48-months-ago +wellons") "wellons")
       ("*" (elfeed-search-toggle-all '*) "toggle *"
        :column "Other")
       ("C-g" nil "quit" :color blue))
     elfeed-search-mode-map)))

(use-package elisp-demos
  :commands (elisp-demos-advice-helpful-update)
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elpy
  ;; https://github.com/davidhalter/jedi/issues/1085
  ;; https://github.com/jorgenschaefer/elpy/issues/1115
  ;; https://github.com/jorgenschaefer/elpy/issues/1123
  ;; https://github.com/jorgenschaefer/elpy/pull/1279
  :after python
  :custom
  (elpy-company-post-completion-function #'elpy-company-post-complete-parens)
  (elpy-modules '(elpy-module-sane-defaults
                  elpy-module-company
                  elpy-module-eldoc))
  (elpy-remove-modeline-lighter nil)
  :commands (elpy-company-post-complete-parens
             elpy-enable)
  :init
  (elpy-enable)
  :delight (elpy-mode " 🐍"))

(use-package elpy-rpc
  :preface
  (defcustom elpy-no-get-completions-rx
    "-?\\([0-9]+\\.?[0-9]*\\|0[Bb][01]+\\|0[Oo][0-8]+\\|0[Xx][0-9A-Fa-f]+\\)"
    "Let `elpy-rpc-get-completions' skip text matching this regexp.
Sometimes the jedi backend returns completions that confuse elpy, e.g.
for numbers.  Extend the regexp in case you find other similar cases
and file a bug report."
    :type 'string
    :group 'elpy)
  :custom
  (elpy-rpc-ignored-buffer-size (lsh 1 18))
  (elpy-rpc-virtualenv-path 'current)
  :commands (elpy-rpc
             elpy-rpc--buffer-contents)
  :config
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
                success error))))

(use-package elisp-mode
  :delight (emacs-lisp-mode "🐮 " :major))

(use-package emms
  ;; Let mpd play most sound, and mpv everything else (ideally video only).
  :custom
  (emms-player-list '(emms-player-mpd emms-player-mpv))
  :commands (emms-player-set)
  :hook
  ((kill-emacs) . emms-stop))

(use-package emms-browser
  :commands (emms-smart-browse))

(use-package emms-info
  :custom
  (emms-info-functions nil))

(use-package emms-info-libtag
  :when (executable-find "emms-print-metadata")
  :after emms-setup
  :commands (emms-info-libtag)
  :config
  (add-to-list 'emms-info-functions 'emms-info-libtag))

(use-package emms-mode-line
  :custom
  (emms-mode-line-format ""))

(use-package emms-player-mpd
  ;; Let mpd play most (ideally all) sound.
  ;; Get the *-bb-mp3 links from the NPO m3u files that mpd fails to grok.
  :after emms-setup
  :custom
  (emms-player-mpd-music-directory "/home/gav/Music")
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6600")
  :commands (emms-info-mpd)
  :config
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; Bug: use of `rx' instead of `emms-player-simple-regexp'
  ;; implies that `case-fold-search' must be non-nil.
  (emms-player-set emms-player-mpd 'regex
                   (rx (or "-bb-mp3"
                           (and "." (or "flac" "m3u" "mp3" "ogg" "opus" "pls" "soundcloud")))
                       eos)))

(use-package emms-player-mpv
  :after emms-setup)

(use-package emms-playing-time
  :custom
  (emms-playing-time-display-format " %s "))

(use-package emms-playlist-mode
  :custom
  (emms-playlist-mode-center-when-go t))

(use-package emms-setup
  :commands (emms-all)
  :defer 2
  :config
  (emms-all))

(use-package emms-streams
  :custom
  ;; To show the current playlist, do "M-x emms".
  (emms-streams-file (no-littering-expand-etc-file-name "emms/streams.el")))

(use-package engine-mode
  ;; https://github.com/asok/.emacs.d/blob/master/inits/init-engine-mode.el
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-engine-mode.el
  ;; https://gitlab.com/ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-engine.el
  :commands (engine-mode
             engine/execute-search
             engine/get-query)
  :init
  (require 'format-spec)
  (engine-mode 1)
  (defengine arch-wiki
    "http://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
    :keybinding "a")
  (defengine duck-duck-go
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine open-streetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "o")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (with-eval-after-load 'hydra
    (bind-key*
     "C-z C-e"
     (defhydra hydra-engine (:color blue)
       "Search"
       ("a" engine/search-arch-wiki      "arch-wiki")
       ("d" engine/search-duck-duck-go   "duck-duck-go")
       ("g" engine/search-github         "github")
       ("o" engine/search-open-streetmap "open-streetmap")
       ("w" engine/search-wikipedia      "wikipedia")))))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eshell
  ;; http://emacshorrors.com/post/life-is-too-much
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  ;; https://github.com/wasamasa/dotemacs/blob/master/init.org#eshell
  :config
  (use-package em-alias
    :custom
    (eshell-aliases-file (no-littering-expand-etc-file-name "eshell/alias")))
  (use-package em-hist
    :custom
    (eshell-hist-ignoredups t)
    (eshell-save-history-on-exit t))
  (use-package em-ls
    :custom
    (eshell-ls-initial-args nil))
  (use-package em-prompt
    :defines (eshell-prompt-regexp))
  (use-package em-term
    :custom
    (eshell-destroy-buffer-when-process-dies t)
    (eshell-visual-commands '("htop"
                              "ipython"
                              "jupyter"
                              "less"
                              "more"
                              "mpv"
                              "ncftp"
                              "tmux"
                              "top"
                              "watch")))
  (use-package esh-mode
    :commands (eshell-life-is-too-much)
    :bind (:map eshell-mode-map
                ("C-d" . (lambda (arg)
                           (interactive "p")
                           (if (and (eolp)
                                    (looking-back eshell-prompt-regexp nil))
                               (eshell-life-is-too-much)
                             (delete-char arg)))))))

(use-package expand-region
  :bind* (("C-=" . er/expand-region)))

(use-package eww
  ;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
  ;; https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode
  ;; https://www.reddit.com/r/emacs/comments/54kczj/reddit_client_for_emacs/
  :preface
  (defcustom eww-readable-sites
    '("www.cnrtl.fr"
      "www.thefreedictionary.com"
      "www.woorden.org")
    "List of urls to show using `eww-readable'."
    :type '(repeat string)
    :group 'eww)
  (defun on-eww-readable ()
    (let ((url (eww-current-url)))
      (when (catch 'found
              (mapc (lambda (site)
                      (when (string-match (regexp-quote site) url)
                        (throw 'found site)))
                    eww-readable-sites)
              nil)
        (eww-readable))))
  (defun on-eww-rename-buffer ()
    (rename-buffer "eww" t))
  (defun reddit-browser ()
    (interactive)
    (eww-browse-url (format "https://www.reddit.com/r/%s/.mobile"
                            (completing-read "sub-reddit: "
                                             '("emacs"
                                               "i3wm"
                                               "orgmode")
                                             nil t))))
  :defines
  eww-link-keymap
  eww-mode-map
  :hook
  ((eww-mode . on-eww-rename-buffer)
   (eww-after-render . on-eww-readable))
  :commands (eww-browse-url
             eww-current-url
             eww-open-filed
             eww-readable))

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "GPG_AGENT_INFO"))
  :commands (exec-path-from-shell-copy-envs
             exec-path-from-shell-initialize)
  :init
  (cd "~")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("SSH_AGENT_PID"
                                    "SSH_AUTH_SOCK"))
  :demand t)

(when (getenv "EXWM")
  (use-package exwm
    ;; https://github.com/DamienCassou/emacs.d/blob/master/init.el
    ;; https://github.com/ch11ng/exwm/wiki
    ;; https://github.com/dakra/dmacs/blob/master/init.org
    ;; https://github.com/technomancy/dotfiles/blob/master/.emacs.d/phil/wm.el
    ;; https://gitlab.com/ambrevar/dotfiles/tree/master/.emacs.d
    :preface
    (require 'dbus)

    (defvar no-ac-display-battery--dbus-object nil
      "D-Bus object remembering the return value of `dbus-register-signal'.
Use this to unregister from the D-BUS.")

    (defun no-ac-display-battery--display-battery-mode ()
      "Hide or show the battery status on AC or battery power."
      (if (dbus-get-property :system
                             "org.freedesktop.UPower"
                             "/org/freedesktop/UPower"
                             "org.freedesktop.UPower"
                             "OnBattery")
          (display-battery-mode)
        (display-battery-mode -1)))

    (defun no-ac-display-battery--start-listening ()
      "Start listening for UPower events."
      (if (not (member "org.freedesktop.UPower" (dbus-list-names :system)))
          (message "Install and/or launch the upower daemon")
        (setq no-ac-display-battery--dbus-object
              (dbus-register-signal
               :system
               "org.freedesktop.UPower"
               "/org/freedesktop/UPower/devices/line_power_AC"
               "org.freedesktop.DBus.Properties"
               "PropertiesChanged"
               (lambda (_interface _changed _invalidated)
                 ;; "Online" is not in _CHANGED when it did not change.
                 (no-ac-display-battery--display-battery-mode))))
        (no-ac-display-battery--display-battery-mode)))

    (defun no-ac-display-battery--stop-listening ()
      "Stop listening for UPower events."
      (when (dbus-unregister-object no-ac-display-battery--dbus-object)
        (setq no-ac-display-battery--dbus-object nil))
      (display-battery-mode -1))

    (define-minor-mode no-ac-display-battery-mode
      "Hide or show the battery status on AC or no AC power."
      :global t
      :init-value nil
      (if no-ac-display-battery-mode
          (no-ac-display-battery--start-listening)
        (no-ac-display-battery--stop-listening)))

    (defcustom my-exwm-teardown-hook nil
      "Hook to power-DOWN or re-BOOT the computer cleanly."
      :type 'hook
      :group 'exwm)

    (defun my-exwm-alsamixer ()
      (interactive)
      (start-process-shell-command "alsamixer" nil "xterm -e alsamixer"))

    (defun my-exwm-invoke (command)
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))

    (defun my-exwm-lock-screen ()
      (interactive)
      (shell-command-to-string "i3lock -c 000000"))

    (defun my-exwm-teardown ()
      "This saves all buffers and runs `kill-emacs-hook' without killing exwm or Emacs."
      (save-some-buffers t)
      ;; `run-hooks' does not work with let binding.
      (setq my-exwm-teardown-hook (thread-last kill-emacs-hook
                                    (remove 'exwm--server-stop)
                                    (remove 'server-force-stop)))
      (run-hooks 'my-exwm-teardown-hook))

    (defun my-exwm-power-down ()
      "Save all Emacs buffers and power-DOWN the computer."
      (interactive)
      (buffer-face-set `(:background ,exwm-tear-down-background-color))
      (when (y-or-n-p "Really want to power-DOWN?")
        (my-exwm-teardown)
        (start-process-shell-command "power-DOWN" nil "sudo shutdown -h -t 2 now"))
      (buffer-face-mode -1))

    (defun my-exwm-re-boot ()
      "Save all Emacs buffers and re-BOOT the computer."
      (interactive)
      (buffer-face-set `(:background ,exwm-tear-down-background-color))
      (when (y-or-n-p "Really want to re-BOOT?")
        (my-exwm-teardown)
        (start-process-shell-command "re-BOOT" nil "sudo shutdown -r -t 2 now"))
      (buffer-face-mode -1))

    (defun no-exwm-window-in-frame-p ()
      "Check for no EXWM window in the selected frame."
      (cl-loop for window being the windows of (selected-frame)
               when (with-current-buffer (window-buffer window)
                      (eq major-mode 'exwm-mode))
               return nil
               finally return t))

    (defun on-exwm-update-class ()
      (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                  (string= "gimp" exwm-instance-name))
        (exwm-workspace-rename-buffer exwm-class-name)))

    (defun on-exwm-update-title ()
      (when (or (not exwm-instance-name)
                (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                (string= "gimp" exwm-instance-name))
        (exwm-workspace-rename-buffer exwm-title)))

    :hook
    (exwm-update-class . on-exwm-update-class)
    (exwm-update-title . on-exwm-update-title)
    :commands (exwm-enable
               exwm-reset)
    :init
    (exwm-enable)
    :config
    (no-ac-display-battery-mode 1)
    (display-time-mode 1)
    (menu-bar-mode 0))

  (use-package exwm-floating
    :custom
    (exwm-floating-border-color "BlueViolet")
    (exwm-floating-border-width 3))

  (use-package exwm-input
    :custom
    ;; Bind `s-' prefix exwm specific keys when exwm gets enabled,
    ;; since those key-bindings may conflict with other window managers.
    (exwm-input-global-keys
     `(([?\s-&] . my-exwm-invoke)
       ([?\s-B] . my-exwm-re-boot)
       ([?\s-D] . my-exwm-power-down)
       ([?\s-a] . my-exwm-alsamixer)
       ([?\s-b] . switch-to-buffer)
       ([?\s-i] . my-exwm-invoke)
       ([?\s-l] . my-exwm-lock-screen)
       ([?\s-o] . ace-window)
       ([?\s-r] . exwm-reset)
       ([?\s-t] . exwm-input-toggle-keyboard)
       ([?\s-w] . exwm-workspace-switch)
       ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-%d" i)) .
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create ,i))))
                 (number-sequence 0 9))))
    :commands (exwm-input-set-key
               exwm-input-toggle-keyboard))

  (use-package exwm-layout
    :custom
    (exwm-layout-show-all-buffers t))

  (use-package exwm-manage
    :custom
    (exwm-manage-configurations
     '(((or (equal "Alacritty" exwm-class-name)
            (equal "XTerm" exwm-class-name)
            (equal "kitty" exwm-class-name))
        char-mode t
        simulation-keys (([?\C-c ?\C-c] . [?\C-c])))
       ((equal "Firefox" exwm-class-name)
        simulation-keys (([?\C-b] . [left])
                         ([?\M-b] . [C-left])
                         ([?\C-f] . [right])
                         ([?\M-f] . [C-right])
                         ([?\C-p] . [up])
                         ([?\C-n] . [down])
                         ([?\C-a] . [home])
                         ([?\C-e] . [end])
                         ([?\M-v] . [prior])
                         ([?\C-v] . [next])
                         ([?\C-d] . [delete])
                         ([?\C-k] . [S-end delete])
                         ;; cut, copy, and paste:
                         ([?\C-w] . [?\C-x])
                         ([?\M-w] . [?\C-c])
                         ([?\C-y] . [?\C-v])
                         ;; search:
                         ([?\C-s] . [?\C-f])
                         ;; close tab instead of quitting Firefox:
                         ([?\C-q] . [?\C-w])))
       ;; Remove the gimp state directory with "rm -rf ~/.config/GIMP".
       ((equal "GNU Image Manipulation Program" exwm-title)
        floating t
        floating-mode-line nil
        height 0.5
        width 0.5)
       ((string-prefix-p "sun-awt-X11-" exwm-instance-name)
        floating t
        floating-mode-line nil))))

  (use-package exwm-randr
    ;; https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
    ;; https://github.com/ch11ng/exwm/wiki
    :preface
    (defun my-exwm-randr-connected-monitors ()
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (save-match-data
          (let (matches)
            (while (re-search-forward
                    "\\(eDP1\\|DP1\\|HDMI1\\|VIRTUAL1\\) connected" nil t)
              (push (match-string-no-properties 1) matches))
            (nreverse matches)))))

    (defun on-exwm-randr-screen-change ()
      (let* ((monitors (my-exwm-randr-connected-monitors))
             (count (length monitors))
             (wop))
        (cond
         ((eq count 2)
          (dotimes (i 10)
            (if (cl-evenp i)
                (setq wop (plist-put wop i (car monitors)))
              (setq wop (plist-put wop i (cadr monitors)))))
          (message "Exwm-randr: configure 2 monitors"))
         ((eq count 1)
          (dotimes (i 10)
            (setq wop (plist-put wop i (car monitors))))
          (message "Exwm-randr: configure 1 monitor")))
        (setq exwm-randr-workspace-monitor-plist wop)))

    :when (string= (system-name) "venus")
    :hook
    (exwm-randr-screen-change . on-exwm-randr-screen-change)
    :commands (exwm-randr-enable)
    :init
    (exwm-randr-enable))

  (use-package exwm-workspace
    :custom
    (exwm-workspace-number 2)
    (exwm-workspace-show-all-buffers t)
    :commands (exwm-workspace-rename-buffer
               exwm-workspace-switch
               exwm-workspace-switch-create)))

(use-package face-remap
  :preface
  (defcustom exwm-tear-down-background-color "DarkRed"
    "EXWM tear down background color."
    :type 'string
    :group 'display)
  (defcustom overwrite-mode-background-color "DarkGreen"
    "Overwrite mode background color."
    :type 'string
    :group 'display)
  :commands (buffer-face-mode
             buffer-face-set)
  :delight (buffer-face-mode))

(use-package faces
  :when window-system
  :commands (invert-face)
  :init
  (invert-face 'default))

(use-package files
  :commands (executable-find
             file-remote-p)
  :custom
  (insert-directory-program (or (executable-find "gls")
                                (executable-find "ls"))))

(use-package flycheck
  ;; https://www.flycheck.org/en/latest/index.html
  :custom (flycheck-check-syntax-automatically (quote (idle-change newline save)))
  :hook ((elpy-mode) . flycheck-mode))

(use-package flymake
  :bind ((:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package flyspell
  ;; See my locale settings for Darwin in my .zprofile and:
  ;; https://apple.stackexchange.com/questions/338638/mojave-and-its-non-respect-of-the-applelocale-preference
  :preface
  (defun toggle-flyspell-dwim-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode 1))))
  :hook
  ((prog-mode) . flyspell-prog-mode)
  ((text-mode) . flyspell-mode)
  :delight (flyspell-mode " ✔"))

(use-package frame
  ;; http://emacsninja.com/posts/making-emacs-more-presentable.html
  :preface
  (defun ninja-alter-frame-font-size (fn)
    (let* ((current-font-name (frame-parameter nil 'font))
           (decomposed-font-name (x-decompose-font-name current-font-name))
           (font-size (string-to-number (aref decomposed-font-name 5))))
      (aset decomposed-font-name 5 (number-to-string (funcall fn font-size)))
      (set-frame-font (x-compose-font-name decomposed-font-name) t t)))
  (defun ninja-inc-frame-font-size ()
    (interactive)
    (ninja-alter-frame-font-size '1+))
  (defun ninja-dec-frame-font-size ()
    (interactive)
    (ninja-alter-frame-font-size '1-))
  (bind-keys* ("C-x C-+" . ninja-inc-frame-font-size)
              ("C-x C--" . ninja-dec-frame-font-size))
  :commands (display-graphic-p
             set-frame-font))

(use-package free-keys
  :custom
  (free-keys-modifiers '("" "C" "M" "C-M" "s"))
  :commands (free-keys))

(use-package goto-addr
  :preface
  (defun toggle-goto-address-dwim-mode ()
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (if goto-address-prog-mode
            (goto-address-prog-mode -1)
          (goto-address-prog-mode 1))
      (if goto-address-mode
          (goto-address-mode -1)
        (goto-address-mode 1))))
  :bind ((:map goto-address-highlight-keymap
               ("<RET>" . goto-address-at-point)
               ("M-<RET>" . new-line)))
  :hook
  ((eshell-mode
    shell-mode) . goto-address-mode)
  ((prog-mode) . goto-address-prog-mode))

(use-package git-commit
  :preface
  (put 'git-commit-major-mode 'safe-local-variable
       (lambda (m) (or (eq m 'gfm-mode)
                       (eq m 'git-commit-elisp-text-mode)
                       (eq m 'markdown-mode)
                       (eq m 'org-mode)
                       (eq m 'text-mode))))
  :custom
  (git-commit-major-mode 'gfm-mode))

(use-package gpastel
  ;; Try to prevent gpaste-daemon from using 100 % cpu time by
  ;; disabling image support.
  :when (eq system-type 'gnu/linux)
  :commands (gpastel-mode)
  :init
  (when (= 0 (call-process-shell-command
              "gsettings list-recursively org.gnome.GPaste"))
    (gpastel-mode)))

(use-package haskell-mode
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  :mode (((rx (seq ".cabal" eos)) . haskell-cabal-mode)
         ((rx (seq ".hs" (opt (or "-boot" "c")) eos)) . haskell-mode)
         ((rx (seq ".lhs" eos)) . literate-haskell-mode))
  :delight (haskell-mode "🍛 " :major))

(use-package helm
  :custom
  (helm-always-two-windows t)
  (helm-reuse-last-window-split-state t)
  (helm-split-window-default-side 'left)
  (helm-split-window-inside-p nil)
  (helm-use-frame-when-dedicated-window t)
  (helm-use-frame-when-more-than-two-windows t))

(use-package helm-adaptive
  :after helm
  :custom
  (helm-adaptive-sort-by-frequent-recent-usage t)
  :commands (helm-adaptive-mode)
  :init
  (helm-adaptive-mode +1))

(use-package helm-buffers
  :custom
  (helm-buffers-fuzzy-matching t)
  :bind ((:map global-map ("C-x x" . helm-mini))))

(use-package helm-command
  :bind ((:map global-map ("M-x" . helm-M-x))))

(use-package helm-config
  :demand t)

(use-package helm-elisp
  :custom
  (helm-apropos-fuzzy-match t))

(use-package helm-eshell
  :custom
  (helm-eshell-fuzzy-match t))

(use-package helm-files
  :custom
  (helm-ff-fuzzy-matching t)
  :bind ((:map global-map
               ("C-x C-f" . helm-find-files)
               ("C-x p" . helm-browse-project)
               ("C-x r p" . helm-projects-history))))

(use-package helm-for-files
  :custom
  (helm-file-cache-fuzzy-match t)
  (helm-recentf-fuzzy-match t))

(use-package helm-grep
  :custom
  (helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  :bind ((:map global-map ("M-g a" . helm-do-grep-ag))))

(use-package helm-imenu
  :custom
  (helm-imenu-fuzzy-match t))

(use-package helm-locate
  :custom
  (helm-locate-fuzzy-match t))

(use-package helm-ls-git
  ;; Prevent recursive loading in case of "make build".
  :no-require t
  :custom
  (helm-ls-git-status-command 'magit-status-internal))

(use-package helm-mode
  :commands (helm-mode)
  :init
  (helm-mode +1)
  :delight (helm-mode " 🎯"))

(use-package helm-net
  :custom
  (helm-net-prefer-curl (if (executable-find "curl") t nil)))

(use-package helm-occur
  :bind ((:map global-map ("M-s o" . helm-occur))))

(use-package helm-ring
  :bind ((:map global-map ("M-y" . helm-show-kill-ring))))

(use-package helm-semantic
  :custom
  (helm-semantic-fuzzy-match t))

(use-package helm-tags
  :custom
  (helm-etags-fuzzy-match t))

(use-package helm-x-files
  :custom
  (helm-session-fuzzy-match t))

(use-package help
  :bind ((:map help-map
               ("M" . describe-minor-mode)))
  :commands (temp-buffer-resize-mode)
  :init
  (temp-buffer-resize-mode 1))

(use-package helpful
  :bind ((:map help-map
               ("M-a" . helpful-at-point)
               ("M-c" . helpful-command)
               ("M-f" . helpful-function)
               ("M-k" . helpful-key)
               ("M-m" . helpful-macro)))
  :commands (helpful-callable
             helpful-variable))

(use-package hercules
  :commands (hercules-def))

(use-package hideshow
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding))
  :hook ((prog-mode) . hs-minor-mode)
  :delight (hs-minor-mode " 🙈"))

(use-package hl-line
  :hook
  ((Info-mode
    elfeed-show-mode
    emms-playlist-mode
    help-mode
    magit-status-mode
    special-mode) . hl-line-mode))

(use-package hydra
  ;; http://oremacs.com/2016/04/04/hydra-doc-syntax/
  :preface
  (bind-key*
   "C-z C-a"
   (defhydra hydra-insert-arrow (:hint none :base-map (make-sparse-keymap))
     "
_7_ _8_ _9_ 🡬 🡩 🡭
_4_ ^ ^ _6_ 🡨   🡪
_1_ _2_ _3_ 🡯 🡫 🡮
"
     ("1" (insert-char ?🡯))
     ("2" (insert-char ?🡫))
     ("3" (insert-char ?🡮))
     ("4" (insert-char ?🡨))
     ("6" (insert-char ?🡪))
     ("7" (insert-char ?🡬))
     ("8" (insert-char ?🡩))
     ("9" (insert-char ?🡭))
     ("C-g" nil nil :color blue)))
  (bind-key*
   "C-z C-r"
   (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                        :color pink
                                        :post (deactivate-mark))
     "
^^^^        [_k_] kill               [_c_] clear    [_N_] number-lines
  ^_p_^     [_w_] copy-as-kill       [_d_] delete   [_m_] toggle-mark
_b_   _f_   [_y_] yank               [_o_] open     [_x_] exchange-point-mark
  ^_n_^     [_r_] copy-to-register   [_t_] string
^^^^        [_g_] insert-register    [_u_] undo     [_C-g_] quit
"
     ("b" backward-char nil)
     ("f" forward-char nil)
     ("p" previous-line nil)
     ("n" next-line nil)

     ("k" kill-rectangle nil)             ;; C-x r k
     ("w" copy-rectangle-as-kill nil)     ;; C-x r M-w
     ("y" yank-rectangle nil)             ;; C-x r y
     ("r" copy-rectangle-to-register nil) ;; C-x r r
     ("g" insert-register nil)            ;; C-x r g

     ("c" clear-rectangle nil)  ;; C-x r c
     ("d" delete-rectangle nil) ;; C-x r d
     ("o" open-rectangle nil)   ;; C-x r o
     ("t" string-rectangle nil) ;; C-x r t
     ("u" undo nil)

     ("N" rectangle-number-lines nil) ;; C-x r N
     ("m" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1)) nil)
     ("x" exchange-point-and-mark nil) ;; C-x C-x

     ("C-g" nil nil :color blue)))

  (bind-key*
   "C-z C-t"
   (defhydra hydra-toggle-mode (:color pink :hint none)
     "
_a_  ?a? auto-fill             _ii_ ?ii? iimage           _vl_ ?vl? visual-line
_c_  ?c? column-number         _it_ ?it? indent-tabs      _vm_ ?vm? view-mode
_d_  ?d? display-line-numbers  _o_  ?o? org-table        _wg_ ?wg? writegood
_fc_ ?fc? flycheck              _r_  ?r? read-only        _wk_ ?wk? which-key
_fl_ ?fl? font-lock             _s_  ?s? smartparens      _ws_ ?ws? white-space
_fs_ ?fs? flyspell
_g_  ?g? goto-address          _tl_ ?tl? truncate-lines   _C-g_  quit
"
     ("a" #'auto-fill-mode
      (if (bound-and-true-p auto-fill-function) "[X]" "[ ]"))
     ("c" #'column-number-mode
      (if (bound-and-true-p column-number-mode) "[X]" "[ ]"))
     ("d" #'display-line-numbers-mode
      (if (bound-and-true-p display-line-numbers-mode) "[X]" "[ ]"))
     ("fc" #'flycheck-mode
      (if (bound-and-true-p flycheck-mode) "[X]" "[ ]"))
     ("fl" #'font-lock-mode
      (if (bound-and-true-p font-lock-mode) "[X]" "[ ]"))
     ("fs" #'toggle-flyspell-dwim-mode
      (if (bound-and-true-p flyspell-mode) "[X]" "[ ]"))
     ("g" #'toggle-goto-address-dwim-mode
      (if (or (bound-and-true-p goto-address-prog-mode)
              (bound-and-true-p goto-address-mode)) "[X]" "[ ]"))
     ("ii" #'iimage-mode
      (if (bound-and-true-p iimage-mode) "[X]" "[ ]"))
     ("it" (setq indent-tabs-mode (not (bound-and-true-p indent-tabs-mode)))
      (if (bound-and-true-p indent-tabs-mode) "[X]" "[ ]"))
     ("o" #'orgtbl-mode
      (if (bound-and-true-p orgtbl-mode) "[X]" "[ ]"))
     ("r" #'read-only-mode
      (if (bound-and-true-p buffer-read-only) "[X]" "[ ]"))
     ("s" #'smartparens-mode
      (if (bound-and-true-p smartparens-mode) "[X]" "[ ]"))
     ("tl" #'toggle-truncate-lines
      (if (bound-and-true-p truncate-lines) "[X]" "[ ]"))
     ("vl" #'visual-line-mode
      (if (bound-and-true-p visual-line-mode) "[X]" "[ ]"))
     ("vm" #'view-mode
      (if (bound-and-true-p view-mode) "[X]" "[ ]"))
     ("wg" #'writegood-mode
      (if (bound-and-true-p writegood-mode) "[X]" "[ ]"))
     ("wk" #'which-key-mode
      (if (bound-and-true-p which-key-mode) "[X]" "[ ]"))
     ("ws" #'whitespace-mode
      (if (bound-and-true-p whitespace-mode) "[X]" "[ ]"))
     ("C-g" nil nil :color blue)))
  :custom
  (hydra-verbose t)
  :commands (hydra--call-interactively-remap-maybe
             hydra-add-font-lock
             hydra-default-pre
             hydra-keyboard-quit
             hydra-set-transient-map
             hydra-show-hint)
  :config
  (hydra-add-font-lock))

(use-package ibuffer
  ;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
  :custom
  (ibuffer-expert nil)
  (ibuffer-saved-filter-groups
   '(("Emacs"
      ("Code" (mode . emacs-lisp-mode))
      ("Doc" (or (mode . Info-mode)
                 (mode . help-mode)
                 (mode . helpful-mode)))
      ("Eshell" (mode . eshell-mode))
      ("Magit" (derived-mode . magit-mode))
      ("Occur" (mode . occur-mode))
      ("EMMS" (or (mode . emms-lyrics-mode)
                  (mode . emms-mark-mode)
                  (mode . emms-playlist-mode)))
      ("EXWM" (mode . exwm-mode)))
     ("Python"
      ("Code" (mode . python-mode))
      ("Doc" (mode . Info-mode))
      ("Eshell" (mode . eshell-mode))
      ("Magit" (derived-mode . magit-mode))
      ("Occur" (mode . occur-mode))
      ("EMMS" (or (mode . emms-lyrics-mode)
                  (mode . emms-mark-mode)
                  (mode . emms-playlist-mode)))
      ("EXWM" (mode . exwm-mode)))
     ("Text"
      ("Org" (mode . org-mode))
      ("TeX" (or (derived-mode . tex-mode)
                 (mode . bibtex-mode)))
      ("PDF" (mode . pdf-view-mode))
      ("Doc" (mode . Info-mode))
      ("Eshell" (mode . eshell-mode))
      ("Magit" (derived-mode . magit-mode))
      ("Occur" (mode . occur-mode))
      ("Code" (or (mode . emacs-lisp-mode)
                  (mode . python-mode)
                  (mode . shell-mode)))
      ("EMMS" (or (mode . emms-lyrics-mode)
                  (mode . emms-mark-mode)
                  (mode . emms-playlist-mode)))
      ("EXWM" (mode . exwm-mode)))))
  (ibuffer-save-with-custom nil)
  :bind ((:map global-map
               ("C-x C-b" . ibuffer)))
  :hook
  ((ibuffer-mode) . ibuffer-auto-mode))

(use-package iedit
  :custom
  (iedit-mode-line
   '(" 🖹:" (:eval
            (format "%d/%d" iedit-occurrence-index (iedit-counter)))))
  (iedit-toggle-key-default nil)
  ;; "Mastering Emacs" recommends this and `lispy' uses this.
  :bind ((:map global-map
               ("M-i" . iedit-mode))
         (:map isearch-mode-map
               ("M-i" . iedit-mode-from-isearch))
         (:map esc-map
               ("M-i" . iedit-execute-last-modification))
         (:map help-map
               ("M-i" . iedit-mode-toggle-on-function)))
  :config
  (hercules-def
   :toggle-funs #'iedit-mode
   :hide-funs #'iedit-quit
   :keymap 'iedit-mode-keymap
   :flatten t))

(use-package iedit-lib
  :commands (iedit-quit))

(use-package ivy
  :disabled
  ;; https://github.com/dakra/dmacs/blob/master/init.org#ivy
  ;; https://github.com/sam217pa/emacs-config
  ;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
  :preface
  (defun ivy-ignore-exwm-buffers (str)
    (let ((buf (get-buffer str)))
      (when buf
        (with-current-buffer buf
          (or
           (file-remote-p (or (buffer-file-name) default-directory))
           (eq major-mode 'exwm-mode))))))
  (defun ivy-ignore-non-exwm-buffers (str)
    (let ((buf (get-buffer str)))
      (if buf
          (with-current-buffer buf
            (or
             (file-remote-p (or (buffer-file-name) default-directory))
             (not (eq major-mode 'exwm-mode))))
        t)))
  (defun ivy-switch-buffer-exwm ()
    "Like ivy-switch-buffer, but show only EXWM buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers '(ivy-ignore-non-exwm-buffers))))
      (ivy-switch-buffer)))
  (defun ivy-switch-buffer-non-exwm ()
    "Like ivy-switch-buffer, but hide all EXWM buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers '(ivy-ignore-exwm-buffers))))
      (ivy-switch-buffer)))
  (defun my-ivy-switch-buffer (p)
    "Like ivy-switch-buffer, but hide all EXWM buffers by default.
With one prefix arg, show only EXWM buffers. With two, show all buffers."
    (interactive "p")
    (cl-case p
      (1 (ivy-switch-buffer-non-exwm))
      (4 (ivy-switch-buffer-exwm))
      (16 (ivy-switch-buffer))))
  :custom
  (ivy-case-fold-search-default 'auto)
  (ivy-count-format "(%d/%d) ")
  (ivy-display-function nil)
  (ivy-height 10)
  (ivy-use-ignore-default t)
  (ivy-use-virtual-buffers t)
  :bind ((:map global-map
               ("C-c C-r" . ivy-resume)
               ("C-x B" . ivy-switch-buffer-other-window)
               ("C-x b" . my-ivy-switch-buffer)))
  :commands (ivy-mode
             ivy-read
             ivy-switch-buffer
             ivy-thing-at-point)
  :init
  (ivy-mode)
  :delight (ivy-mode " 𝝓"))

(use-package ivy-posframe
  :disabled
  :after ivy
  :custom
  (ivy-posframe-border-width 2)
  (ivy-posframe-height (1+ ivy-height))
  (ivy-posframe-width 80)
  (ivy-posframe-min-height (1+ ivy-height))
  (ivy-posframe-min-width 80)
  (ivy-posframe-hide-minibuffer t)
  (ivy-posframe-parameters '((left-fringe . nil)
                             (right-fringe . 0)))
  (ivy-posframe-style 'window-center)
  :custom-face
  (ivy-posframe ((t (:foreground "LawnGreen" :background "Black"))))
  (ivy-posframe-border ((t (:background "BlueViolet"))))
  (ivy-posframe-cursor ((t (:background "LawnGreen"))))
  :commands (ivy-posframe-mode)
  :delight (ivy-posframe-mode " 🗔"))

(use-package ivy-prescient
  :disabled
  :after ivy
  :commands (ivy-prescient-mode)
  :init
  (ivy-prescient-mode))

(use-package jupyter
  ;; Load `jupyter' after `org' and `pyenv-mode' to append `jupyter'
  ;; as the last element of `org-babel-load-language' in a nice
  ;; environment.
  :after (org pyenv-mode)
  :demand t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((jupyter . t)))))

(use-package jupyter-repl
  ;; Looks nice with 'c.interactive.colors = "Linux"' in
  ;; ipython_kernel_config.py.
  :custom-face
  (jupyter-repl-input-prompt
   ((((class color) (background dark)) :foreground "LightGreen")
    (((class color) (background light)) :foreground "DarkGreen")))
  (jupyter-repl-output-prompt
   ((((class color) (background dark)) :foreground "OrangeRed")
    (((class color) (background light)) :foreground "VioletRed")))
  (jupyter-repl-traceback
   ((((class color) (background dark)) :background "DimGrey")
    (((class color) (background light)) :background "LightGrey")))
  :commands (jupyter-run-repl))

(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c e" . macrostep-expand)))
  :config
  (hercules-def
   :show-funs #'macrostep-expand
   :hide-funs #'macrostep-mode
   :keymap 'macrostep-keymap
   :flatten t)
  (use-package use-package))

(use-package magit
  ;; https://stackoverflow.com/questions/4114095/how-to-revert-a-git-repository-to-a-previous-commit
  ;; https://stackoverflow.com/questions/9529078/how-do-i-use-git-reset-hard-head-to-revert-to-a-previous-commit
  :bind ((:map global-map
               ("C-x g"   . magit-status)
               ("C-x M-g" . magit-dispatch)))
  :commands (magit-add-section-hook)
  :config
  ;; https://github.com/dakra/dmacs/blob/master/init.org#magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-ignored-files
                          'magit-insert-untracked-files
                          nil))

(use-package magit-submodule
  :custom
  (magit-submodule-remove-trash-gitdirs t))

(use-package mailcap
  :if (eq system-type 'darwin)
  :commands (mailcap-add)
  :config
  (mailcap-add "application/pdf" #'pdf-view-mode #'window-system))

(use-package man
  :custom
  (Man-width 80))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package monky
  ;; Try chg, since using `cmdserver' as `monky-process-type' fails
  ;; with mercurial-5.0.2 and emacs-27.0.9999.
  :custom
  (monky-hg-executable (or (executable-find "chg")
                           (executable-find "hg")))
  (monky-repository-paths
   `(("dotfiles" . ,(expand-file-name "~/VCS/dotfiles"))
     ("fudge" . ,(expand-file-name "~/VCS/fudge"))
     ("gav-gentoo" . ,(expand-file-name "~/VCS/gav-gentoo"))
     ("gav-overlay" . ,(expand-file-name "~/VCS/gav-overlay"))
     ("ngccdr" . ,(expand-file-name "~/VCS/ngccdr")))))

(use-package minibuffer
  :custom
  (completion-styles (quote (flex))))

(use-package nov
  :mode ((rx (seq ".epub" eos)) . nov-mode))

(use-package novice
  ;; https://www.emacswiki.org/emacs/DisabledCommands
  :preface
  (defun enable-me (&rest _args)
    "Called when a disabled command is executed.
Enable it and reexecute it."
    (put this-command 'disabled nil)
    (message "You typed %s.  %s was disabled.  It ain't no more."
             (key-description (this-command-keys)) this-command)
    (sit-for 0)
    (call-interactively this-command))
  :init
  (setq disabled-command-function #'enable-me))

(use-package ob-core
  :custom
  (org-confirm-babel-evaluate nil)
  :commands (org-babel-execute-src-block))

(use-package ob-lisp
  :custom
  (org-babel-lisp-eval-fn #'sly-eval))

(use-package ob-python
  :custom
  (org-babel-python-command "python -E"))

(use-package ol
  :custom
  (org-link-abbrev-alist
   '(;; Google.
     ("gg-fr" . "https://www.google.fr/search?ie=UTF-8&oe=UTF-8&q=")
     ("gg-nl" . "https://www.google.nl/search?ie=UTF-8&oe=UTF-8&q=")
     ("gg-us" . "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=")
     ;; Google maps.
     ("gm-fr" . "https://maps.google.fr/maps?q=%s")
     ("gm-nl" . "https://maps.google.nl/maps?q=%s")
     ("gm-us" . "https://maps.google.com/maps?q=%s")
     ;; Open street map.
     ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1"))))

(use-package org
  :preface
  (defun find-broken-org-file-links ()
    "Find broken org-mode file links in an org-mode buffer."
    (interactive)
    (if (eq major-mode 'org-mode)
        (let ((paths
               (org-element-map
                   (org-element-parse-buffer 'object) 'link
                 (lambda (link)
                   (let ((path (org-element-property :path link))
                         (type (org-element-property :type link)))
                     (when (equal type "file")
                       (unless (file-exists-p path) path)))))))
          (if paths
              (message "Found broken org-mode file links:\n%s"
                       (mapconcat #'identity paths "\n"))
            (message "Found no broken org-mode file links")))
      (message "Failed to find broken links (major mode is not org-mode)")))

  (defun on-org-mode-eval-blocks ()
    "Evaluate all org-mode source blocks named `org-mode-hook-eval-block'."
    (interactive)
    (if (eq major-mode 'org-mode)
        (let ((blocks
               (org-element-map
                   (org-element-parse-buffer) 'src-block
                 (lambda (element)
                   (when (string= "org-mode-hook-eval-block"
                                  (org-element-property :name element))
                     element)))))
          (dolist (block blocks)
            (goto-char (org-element-property :begin block))
            (org-babel-execute-src-block)))))
  :custom
  (org-adapt-indentation nil)
  (org-babel-load-languages (quote ((calc . t)
                                    (emacs-lisp . t)
                                    (eshell . t)
                                    (gnuplot . t)
                                    (latex . t)
                                    (lisp . t)
                                    (org . t)
                                    (python . t)
                                    (shell . t))))
  (org-catch-invisible-edits 'show-and-error)
  (org-export-backends '(ascii beamer icalendar html md latex man odt org texinfo))
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . (lambda (path link)
                                        (message "Open %s" link)
                                        (eww-open-file path)))
                   ("\\.pdf\\'" . emacs)))
  (org-latex-default-packages-alist
   '(("AUTO"                 "inputenc"     t ("pdflatex"))
     ("T1"                   "fontenc"      t ("pdflatex"))
     (""                     "amsmath"      t)
     (""                     "amssymb"      t)
     (""                     "textcomp"     t)
     (""                     "fontspec"     t ("lualatex"))
     (""                     "unicode-math" t ("lualatex"))
     (""                     "graphicx"     t)
     (""                     "grffile"      t)
     (""                     "longtable"    nil)
     (""                     "wrapfig"      nil)
     (""                     "rotating"     nil)
     ("normalem"             "ulem"         t)
     (""                     "capt-of"      nil)
     ("hyperfootnotes=false" "hyperref"     nil)))
  (org-modules
   '(ol-bibtex
     ol-eshell
     ol-eww
     ol-info
     org-id
     org-protocol))
  (org-src-fontify-natively t)
  (org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "DEFERRED" "ZAPPED"))))
  (org-use-sub-superscripts '{})
  :bind ((:map global-map
               ("C-c a"   . org-agenda)
               ("C-c c"   . org-capture)
               ("C-c l"   . org-store-link)
               ("C-c C-l" . org-insert-link-global))
         (:map org-mode-map
               ("M-q" . org-fill-paragraph)))
  :mode ((rx ".org" eos) . org-mode)
  :hook
  (org-mode . on-org-mode-eval-blocks)
  :commands (org-babel-do-load-languages
             org-link-set-parameters
             org-narrow-to-block
             org-narrow-to-subtree))

(use-package org-agenda
  :after org
  :custom
  (org-agenda-exporter-settings '((ps-landscape-mode t)
                                  (ps-number-of-columns 2)
                                  (ps-paper-type 'a4)
                                  (ps-print-color-p nil)
                                  (ps-print-header nil)))
  (org-agenda-files '("~/VCS/pim/jobs.org"))
  (org-agenda-span 70)
  :demand t)

(use-package org-capture
  :after org
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/tmpfs/tasks.org" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("p" "Protocol" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %? [[%:link][%:description]] \nCaptured On: %U")))
  :demand t)

(use-package org-element
  :functions (org-element-map
              org-element-parse-buffer
              org-element-property))

(use-package org-ref
  :after org
  :custom
  (org-ref-bibliography-notes "~/VCS/research/notes/notes.org")
  (org-ref-cite-color "LawnGreen")
  (org-ref-ref-color "OrangeRed")
  (org-ref-label-color "DeepPink")
  (org-ref-default-bibliography '("~/VCS/research/refs.bib"))
  (org-ref-pdf-directory '("~/VCS/research/papers"))
  :demand t)

(use-package org-ref-bibtex
  :bind ((:map org-mode-map
               ("C-c j" . org-ref-bibtex-hydra/body))))

(use-package org-ref-core
  :bind ((:map org-mode-map
               ("C-c ]" . org-ref-insert-link))))

(use-package org-ref-glossary
  :commands (or-follow-glossary)
  :config
  ;; Short-circuit org-ref-link-set-parameters in org-ref-utils:
  (org-link-set-parameters "ac*"
                           :follow #'or-follow-glossary
                           :face 'org-ref-glossary-face
                           :help-echo 'or-glossary-tooltip
                           :export (lambda (path _ format)
                                     (cond
                                      ((eq format 'latex)
                                       (format "\\gls*{%s}" path))
                                      (t
                                       (format "%s" path))))))

(use-package org-tanglesync
  :bind (("C-c M-a" . org-tanglesync-process-buffer-automatic)
         ("C-c M-i" . org-tanglesync-process-buffer-interactive)
         ("C-c t" . org-tanglesync-mode)))

(use-package org-src
  :custom
  (org-edit-src-content-indentation
   0 "Preserve Python code block indentation.")
  (org-src-preserve-indentation
   t "Preserve Python code block indentation.")
  (org-src-window-setup
   'current-window "Show edit buffer in current window.")
  :commands (org-edit-src-code))

(use-package ox
  :custom
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{}))

(use-package ox-latex
  :custom
  (org-latex-caption-above nil)
  (org-latex-compiler "lualatex")
  (org-latex-hyperref-template nil)
  (org-latex-logfiles-extensions '("blg" "lof" "log" "lot" "out" "toc"))
  (org-latex-pdf-process
   '("lualatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b.aux"
     "lualatex -interaction nonstopmode -output-directory %o %f"
     "lualatex -interaction nonstopmode -output-directory %o %f"))
  ;; Requires CUSTOM_ID property to suppress LaTeX section labels.
  (org-latex-prefer-user-labels t))

(use-package pdf-tools
  :custom
  (pdf-annot-activate-created-annotations t)
  :commands (pdf-tools-install)
  :config
  (pdf-tools-install t))

(use-package pdf-view
  :custom
  (pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("C-r" . isearch-backward)
              ("C-s" . isearch-forward)
              ("M-w" . pdf-view-kill-ring-save))
  :magic
  ("%PDF" . pdf-view-mode))

(use-package peep-dired
  :after dired
  :custom
  (peep-dired-cleanup-eagerly t)
  (peep-dired-cleanup-on-disable t)
  :bind ((:map dired-mode-map
               ("M-s p" . peep-dired))))

(use-package posframe
  :disabled
  :commands (posframe-workable-p)
  :config
  (when (fboundp 'no-exwm-window-in-frame-p)
    (advice-add 'posframe-workable-p
                :before-while #'no-exwm-window-in-frame-p)))

(use-package prescient
  :commands (prescient-persist-mode)
  :config
  (prescient-persist-mode))

(use-package psession
  :demand t
  :init
  (psession-mode +1)
  (psession-savehist-mode +1))

(use-package pyenv-mode
  ;; Loads `elpy' and `python' automatically.
  :preface
  (defun update-pyenv-mode-environment (&rest _)
    (let ((version (pyenv-mode-version))
          (root (pyenv-mode-root)))
      (if (member version '(nil "system"))
          (mapcar #'setenv '("PYTHONDIR"
                             "JUPYTER_CONFIG_DIR"
                             "JUPYTER_DATA_DIR"
                             "JUPYTER_PATH"
                             "JUPYTER_RUNTIME_DIR"))
        (list
         (setenv "IPYTHONDIR"
                 (expand-file-name
                  (concat "~/.ipython-" version)))
         (setenv "JUPYTER_CONFIG_DIR"
                 (expand-file-name
                  (concat root "/versions/" version "/etc/jupyter")))
         (setenv "JUPYTER_DATA_DIR"
                 (expand-file-name
                  (concat "~/.local/share/jupyter-" version)))
         (setenv "JUPYTER_PATH"
                 (expand-file-name
                  (concat root "/versions/" version "/share/jupyter")))
         (setenv "JUPYTER_RUNTIME_DIR"
                 (expand-file-name
                  (concat "~/tmpfs/jupyter-" version)))))))
  :commands (pyenv-mode-root
             pyenv-mode-set
             pyenv-mode-unset
             pyenv-mode-version)
  :defer 2
  :config
  (advice-add 'pyenv-mode-set
              :after #'update-pyenv-mode-environment)
  (advice-add 'pyenv-mode-unset
              :after #'update-pyenv-mode-environment)
  (pyenv-mode-set "3.8.1"))

(use-package python
  :custom
  (python-shell-interpreter-args "-E -i")
  :interpreter ("python" . python-mode)
  :mode ((rx (seq ".py" (opt "w") eos)) . python-mode)
  :delight (python-mode "🐍 " :major))

(use-package rainbow-mode
  :custom
  (rainbow-x-colors-major-mode-list '(emacs-lisp-mode
                                      ielm-mode
                                      lisp-interaction-mode
                                      c-mode
                                      c++-mode
                                      java-mode))
  :hook
  ((emacs-lisp-mode
    ielm-mode
    latex-mode
    lisp-interaction-mode) . rainbow-mode)
  :delight (rainbow-mode " 🌈"))

(use-package recentf
  :after no-littering
  :custom
  (recentf-max-saved-items 100)
  :config
  (mapc #'(lambda (element) (add-to-list 'recentf-exclude element))
        `(no-littering-etc-directory
          no-littering-var-directory
          ,(expand-file-name "~/.orhc-bibtex-cache")
          "/\\.git/.*\\'"
          "/\\.hg/.*\\'"
          "^/\\(?:ssh\\|su\\|sudo\\)?:")))

(use-package reveal
  ;; info -> magit -> FAQ -> FAQ - Issues and Errors.
  :hook
  ((magit-diff-visit-file) . reveal-mode)
  :delight (reveal-mode " 👀"))

(use-package replace
  ;; https://masteringemacs.org/article/searching-buffers-occur-mode
  :preface
  (defun multi-occur-with-this-mode (regexp &optional nlines)
    "Show all lines matching REGEXP in buffers with this major mode."
    (interactive
     (occur-read-primary-args))
    (occur-1 regexp nlines
             (cl-loop
              for buffer being the buffers when
              (eq (buffer-local-value 'major-mode buffer) major-mode)
              collect buffer)))
  :custom
  (list-matching-lines-default-context-lines 0)
  :hook
  ((occur) . occur-rename-buffer)
  :commands (occur-1
             occur-read-primary-args))

(use-package saveplace
  :commands (save-place-mode)
  :config
  (save-place-mode))

(use-package selectrum
  :disabled
  :commands (selectrum-mode)
  :init
  (selectrum-mode +1))

(use-package selectrum-prescient
  :disabled
  :after selectrum
  :commands (selectrum-prescient-mode)
  :init
  (selectrum-prescient-mode +1))

(use-package shr
  :custom
  (shr-max-image-proportion 0.8)
  :commands (shr-browse-url))

(use-package simple
  :preface
  (defun on-overwrite-mode-toggle ()
    "Toggle background-color on overwrite-mode toggle."
    (if (bound-and-true-p overwrite-mode)
        (buffer-face-set `(:background ,overwrite-mode-background-color))
      (buffer-face-mode -1)))
  :custom
  (eval-expression-print-length nil)
  (kill-do-not-save-duplicates t)
  :commands (column-number-mode
             region-active-p)
  :hook
  ((text-mode) . turn-on-auto-fill)
  ((overwrite-mode) . on-overwrite-mode-toggle)
  :config
  (column-number-mode))

(use-package sly
  ;; https://github.com/LispCookbook/cl-cookbook
  ;; https://lispcookbook.github.io/cl-cookbook/emacs-ide.html
  ;; shell% cd ~/.emacs/var
  ;; shell% sbcl
  ;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;; * (save-lisp-and-die "sbcl.core-for-sly")
  :custom
  (common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec-7.0/HyperSpec/")
  (inferior-lisp-program (executable-find "sbcl"))
  (sly-lisp-implementations
   `((sbcl (,inferior-lisp-program
            "--core"
            ,(no-littering-expand-var-file-name "sbcl.core-for-sly")))))
  :bind ((:map sly-prefix-map
               ;; C-c M-h
               ("M-h" . sly-documentation-lookup)))
  :hook
  ((sly-mode) . (lambda ()
                  (unless (sly-connected-p)
                    (save-excursion (sly)))))
  :commands (sly
             sly-connected-p
             sly-eval))

(use-package smartparens
  ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
  ;; https://github.com/ebzzry/dotfiles/blob/master/emacs/fkd/klavoj.el
  :bind ((:map smartparens-mode-map
               ;; I have copied sp-smartparens-bindings.
               ("C-M-f" . sp-forward-sexp)
               ("C-M-b" . sp-backward-sexp)
               ("C-M-d" . sp-down-sexp)
               ("C-M-a" . sp-backward-down-sexp)
               ("C-S-d" . sp-beginning-of-sexp)
               ("C-S-a" . sp-end-of-sexp)
               ("C-M-e" . sp-up-sexp)
               ("C-M-u" . sp-backward-up-sexp)
               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)
               ("C-M-k" . sp-kill-sexp)
               ("C-M-w" . sp-copy-sexp)
               ("M-<delete>" . sp-unwrap-sexp)
               ("M-<backspace>" . sp-backward-unwrap-sexp)
               ("C-<right>" . sp-forward-slurp-sexp)
               ("C-<left>" . sp-forward-barf-sexp)
               ("C-M-<left>" . sp-backward-slurp-sexp)
               ("C-M-<right>" . sp-backward-barf-sexp)
               ("M-D" . sp-splice-sexp)
               ("C-M-<delete>" . sp-splice-sexp-killing-forward)
               ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
               ("C-S-<backspace>" . sp-splice-sexp-killing-around)
               ("C-]" . sp-select-next-thing-exchange)
               ("C-M-]" . sp-select-next-thing)
               ("C-M-SPC" . sp-mark-sexp)
               ("M-F" . sp-forward-symbol)
               ("M-B" . sp-backward-symbol)))
  :hook
  ((prog-mode) . smartparens-strict-mode)
  :config
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  :delight (smartparens-mode (" 🗘" (:eval (if smartparens-strict-mode "/s" s)))))

(use-package smartparens-config
  :demand t)

(use-package subr
  ;; https://github.com/dakra/dmacs/blob/master/init.org
  ;; https://gitlab.com/jabranham/emacs/blob/master/init.el
  ;; https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  :preface
  (defun narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.

Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  With prefix P, don't widen, just narrow
even if buffer is already narrowed."
    (interactive "P")
    (declare (interactive-only t))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing command.
           ;; Remove this first conditional if you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))
  (bind-keys* ("C-x n" . narrow-or-widen-dwim))
  :commands (add-hook
             add-to-list
             derived-mode-p
             eval-after-load
             narrow-to-defun
             narrow-to-region))

(use-package swiper
  :custom
  (swiper-action-recenter t))

(use-package synosaurus
  :custom
  (synosaurus-choose-method 'default)
  :bind* (("C-z C-s l" . synosaurus-lookup)
          ("C-z C-s r" . synosaurus-choose-and-replace)))

(use-package thingatpt
  :functions (thing-at-point-looking-at))

(use-package time
  :custom
  (display-time-format (when (getenv "EXWM")
                         " %R %F")))

(use-package toc-org
  :hook
  ((org-mode) . toc-org-mode))

(use-package tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package transient
  :after magit
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected))
  :custom-face
  (transient-argument ((t :inherit font-lock-warning-face :underline t))))

(use-package unfill
  :bind ((:map global-map
               ("M-q" . unfill-toggle))))

(use-package uniquify
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :custom
  (uniquify-buffer-name-style 'forward)
  :defer 2)

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))

(use-package which-key
  :delight (which-key-mode))

(use-package with-editor
  :hook
  ((eshell-mode
    shell-mode) . with-editor-export-editor))

(use-package wordnut
  :bind* (("C-z C-w" . wordnut-search)))

(use-package ws-butler
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  ((prog-mode
    text-mode) . ws-butler-mode)
  :delight (ws-butler-mode " 🍴"))

(use-package yasnippet
  :custom
  (yas-alias-to-yas/prefix-p nil)
  :commands (yas-expand-from-trigger-key
             yas-global-mode)
  :defer 2
  ;; I fail to use alternative keys in yas-keymap and yas-minor-mode-map as explained in
  ;; https://github.com/capitaomorte/yasnippet/blob/master/doc/faq.org.
  ;; However, everything works fine, sofar.
  :config
  (yas-global-mode 1)
  :delight (yas-minor-mode " ✀"))

(use-package zop-to-char
  :bind ((:map global-map
               ("M-z" . zop-up-to-char)
               ("M-Z" . zop-to-char))))

(progn                                  ; startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (fringe-mode '(nil . 0))  ; left-only
              (setq-default indicate-buffer-boundaries 'left)
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn                                  ; personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
