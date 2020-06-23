;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Early birds

(defcustom use-helm-or-selectrum 'use-selectrum
  "Use helm-mode or selectrum-mode."
  :type '(choice (const :tag "Helm" 'use-helm)
                 (const :tag "Selectrum" 'use-selectrum))
  :group 'emacs)

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
                (defun on-after-init-hook-delay-reset ()
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

(progn                                  ; `use-package'
  (defvar use-package-enable-imenu-support t
    "If non-nil, cause imenu to see `use-package' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to
include support for finding `use-package' and `require' forms.

Must be set before loading use-package.")
  (require 'use-package)
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
  :mode ((rx (seq ".bib" eos)) . bibtex-mode)
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path '("~/VCS/research/papers"))
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org")
  (bibtex-user-optional-fields
   '(("abstract")
     ("doi" "Digital Object Identifier")
     ("url" "Universal Ressource Locator"))))

(use-package font-latex
  :custom
  (font-latex-fontify-sectioning 'color))

(use-package latex
  :mode ((rx (seq ".tex" eos)) . TeX-latex-mode)
  :custom
  (LaTeX-electric-left-right-brace nil "Let smartparens handle braces")
  :hook
  ((LaTeX-mode) . LaTeX-math-mode)
  :commands (LaTeX-narrow-to-environment))

(use-package reftex
  :hook
  ((LaTeX-mode) . reftex-mode)
  :delight (reftex-mode " 📑"))

(use-package reftex-vars
  :custom
  (reftex-default-bibliography "~/VCS/research/refs.bib")
  (reftex-plug-into-AUCTeX t))

(use-package tex
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
  :demand t)

;; alphabetical order
(use-package alert
  :custom
  (alert-default-style 'libnotify))

(use-package autorevert
  :hook
  ((dired-mode) . auto-revert-mode)
  :delight (auto-revert-mode))

(use-package avy
  :disabled
  :custom
  (avy-all-windows t)
  :commands (avy-setup-default)
  :bind* (("C-:" . avy-goto-word-1))
  :init
  (avy-setup-default))

(use-package browse-url
  :unless noninteractive
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
    (browse-url (concat "http://www.cnrtl.fr/definition/" word)))

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
                                  (executable-find "firefox")))
  :commands (browse-url
             browse-url-generic))

(use-package deadgrep
  :bind ((:map global-map
               ("M-g d" . deadgrep))
         (:map deadgrep-mode-map
               ("C-c C-w" . deadgrep-edit-mode))))

(use-package company
  ;; https://github.com/CeleritasCelery/emacs.d/blob/master/emacs.org
  :unless noninteractive
  :custom
  (company-show-numbers t)
  :bind ((:map company-active-map
               ("<return>" . nil)
               ("C-j" . company-complete-selection)
               ("C-m" . nil)
               ))
  :hook
  ((LaTeX-mode
    emacs-lisp-mode
    ielm-mode
    org-mode
    shell-mode
    sly-mode
    sly-mrepl-mode) . company-mode)
  :delight (company-mode " 👫"))

(use-package company-native-complete
  :after native-complete
  :demand t)

(use-package company-prescient
  :hook
  ((company-mode) . company-prescient-mode))

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

(use-package dbus
  :commands (dbus-get-property
             dbus-list-names
             dbus-register-signal
             dbus-unregister-object))

(use-package diff-hl
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :custom
  (diff-hl-draw-borders nil)
  :hook
  ((dired-mode) . diff-hl-dired-mode)
  ((magit-post-refresh) . diff-hl-magit-post-refresh))

(use-package dired
  :unless noninteractive
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

(use-package djvu
  :when (cl-loop for command in '("ddjvu" "djview" "djvm" "djvused")
                 unless (executable-find command)
                 return nil
                 finally return t)
  :mode ((rx (seq ".djvu" eos)) . djvu-dummy-mode))

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
  :commands (electric-layout-mode)
  :config
  (add-hook 'python-mode-hook
            (defun on-python-mode-hook-electric-layout ()
              (make-local-variable 'electric-layout-rules)
              (add-to-list 'electric-layout-rules
                           (cons ?: #'my-python-electric-newline))
              (electric-layout-mode))))

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
  :custom
  (elfeed-db-directory "~/SYNC/elfeed/db")
  (elfeed-feeds
   '(("http://www.cachestocaches.com/feed" g-stein)
     ("http://emacshorrors.com/feed.atom" schneidermann)
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
     ("https://protesilaos.com/codelog.xml" p-stavrou)
     ("https://realpython.com/atom.xml" python)
     ("https://sciencescitoyennes.org/feed/" science)
     ("https://vxlabs.com/index.xml" vxlabs)
     ("https://www.aclu.org/taxonomy/feed-term/2152/feed" aclu)
     ("https://www.bof.nl/rss/" bof)
     ("https://www.democracynow.org/podcast-video.xml" dn)
     ("https://www.laquadrature.net/fr/rss.xml" lqdn)
     ("https://www.lemonde.fr/blog/huet/feed/" science)))
  (elfeed-enclosure-default-dir (expand-file-name "~/tmpfs/"))
  :bind* (("C-x w" . elfeed+db-load+update))
  :commands (elfeed
             elfeed-update)
  :config
  (make-directory elfeed-db-directory t))

(use-package elfeed-db
  :commands (elfeed-db-load
             elfeed-db-save))

(use-package elfeed-search
  :preface
  (defun elfeed-db-save+quit ()
    (interactive)
    (elfeed-db-save)
    (quit-window))
  :bind ((:map elfeed-search-mode-map
               ("?" . describe-mode)
               ("q" . elfeed-db-save+quit)))
  :commands (elfeed-search-set-filter
             elfeed-search-toggle-all
             elfeed-search-update))

(use-package elfeed-show
  :bind ((:map elfeed-show-mode-map
               ("?" . describe-mode)))
  :hook ((elfeed-show) . (lambda ()
                           (setq-local shr-max-image-proportion 0.6))))

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
  :commands (elpy-enable)
  :custom
  (elpy-company-post-completion-function 'elpy-company-post-complete-parens)
  (elpy-modules '(elpy-module-sane-defaults
                  elpy-module-company
                  elpy-module-eldoc))
  (elpy-remove-modeline-lighter nil)
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
                   (rx "." (or "flac" "m3u" "mp3" "ogg" "opus" "pls" "soundcloud") eos)))

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

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package epkg
  ;; https://github.com/dakra/dmacs/blob/master/init.org#unsortet-stuff-in-no-packages
  :config
  (when (and (fboundp 'eieio-oref)
             (fboundp 'epkg))
    (defun borg-check-drone-urls ()
      "Check all drones for outdated upstream urls."
      (interactive)
      (let (moved)
        (dolist (drone (borg-clones))
          (let ((a (borg-get drone "url"))
                (b (ignore-errors (eieio-oref (epkg drone) 'url))))
            (when (and a b (not (forge--url-equal a b)))
              (push (list drone a b) moved))))
        (when moved
          (message (concat "These upstream repositories appear to have moved:\n"
                           (mapconcat (pcase-lambda (`(,drone ,a ,b))
                                        (format "%s: %s => %s" drone a b))
                                      moved "\n"))))))))

(use-package eshell
  ;; http://emacshorrors.com/post/life-is-too-much
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  ;; https://github.com/wasamasa/dotemacs/blob/master/init.org#eshell
  :unless noninteractive
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
                              "less"
                              "more"
                              "mpv"
                              "ncftp"
                              "tmux"
                              "top"
                              "watch")))
  (use-package esh-mode
    :commands (eshell-life-is-too-much)
    :bind ((:map eshell-mode-map
                 ("C-d" . (lambda (arg)
                            (interactive "p")
                            (if (and (eolp)
                                     (looking-back eshell-prompt-regexp nil))
                                (eshell-life-is-too-much)
                              (delete-char arg))))))))

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
  :commands (eww-browse-url
             eww-current-url
             eww-open-file
             eww-readable)
  :config
  (add-hook 'eww-mode-hook
            (defun on-eww-mode-hook-rename-buffer ()
              (rename-buffer "eww" t)))
  (add-hook 'eww-after-render-hook
            (defun on-eww-after-render-hook-make-readable ()
              (let ((url (eww-current-url)))
                (when (catch 'found
                        (mapc
                         (lambda (site)
                           (when (string-match (regexp-quote site) url)
                             (throw 'found site)))
                         eww-readable-sites)
                        nil)
                  (eww-readable))))))

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

(when (and (getenv "EXWM") (not noninteractive))
  (use-package exwm
    ;; https://github.com/DamienCassou/emacs.d/blob/master/init.el
    ;; https://github.com/ch11ng/exwm/wiki
    ;; https://github.com/dakra/dmacs/blob/master/init.org
    ;; https://github.com/technomancy/dotfiles/blob/master/.emacs.d/phil/wm.el
    ;; https://gitlab.com/ambrevar/dotfiles/tree/master/.emacs.d
    :preface

    ;; Handle pointer devices using xinput.
    (defun xpointer--pointer-device-names (goal)
      (with-temp-buffer
        (call-process "xinput" nil t nil "list" "--name-only")
        (cl-loop with output = (buffer-string)
                 for line in (split-string output "\n")
                 when (string-match-p goal line)
                 collect line)))

    (defun xpointer-mouse-names ()
      (xpointer--pointer-device-names (rx "mouse")))

    (defun xpointer-touchpad-names ()
      (xpointer--pointer-device-names (rx (or "glidepoint" "touchpad"))))

    (defcustom xpointer-mouse-name nil
      "Device name of the `xinput' mouse."
      :set (lambda (symbol _value)
             (set-default symbol
                          (car (xpointer-mouse-names))))
      :type '(choice
              (const "Logitech USB Mouse")
              (const "Logitech USB Optical Mouse"))
      :group 'exwm)

    (defcustom xpointer-touchpad-name nil
      "Device name of the `xinput' touch-pad."
      :set (lambda (symbol _value)
             (set-default symbol
                          (car (xpointer-touchpad-names))))
      :type '(choice
              (const "PS/2 Synaptics TouchPad")
              (const "SynPS/2 Synaptics TouchPad"))
      :group 'exwm)

    (defun xpointer--set-device-enabled-to (name to)
      (cl-assert (member to '("0" "1")))
      (if (zerop (call-process "xinput" nil nil nil
                               "--set-prop" name "Device Enabled" to))
          (if (equal "0" to)
              (message "Set `%s' to disabled" name)
            (message "Set `%s' to enabled" name))
        (message "Fail to set `%s' `Device Enabled' to `%s'" name to)))

    (defun xpointer--device-enabled-p (name)
      (setq name (format "%s" name))	; force name to be a string
      (with-temp-buffer
        (if (zerop (call-process "xinput" nil t nil "list-props" name))
            (let ((goal (rx "Device Enabled" (+ any) (group (or "0" "1") eos))))
              (cl-loop with output = (buffer-string)
                       for line in (split-string output "\n")
                       when (string-match goal line)
                       return (match-string-no-properties 1 line)))
          (message "Fail to call `xinput' to read the `%s' status" name))))

    (defun xpointer--disable-device (name)
      (let ((status (xpointer--device-enabled-p name)))
        (cond
         ((equal "0" status)
          (message "No need to set `%s' to disabled" name))
         ((equal "1" status)
          (xpointer--set-device-enabled-to name "0"))
         ((not status)
          (message "Fail to parse the `%s' `Device Enabled' status" name))
         (t status))))

    (defun xpointer--enable-device (name)
      (let ((status (xpointer--device-enabled-p name)))
        (cond
         ((equal "0" status)
          (xpointer--set-device-enabled-to name "1"))
         ((equal "1" status)
          (message "No need to set `%s' to enabled" name))
         ((not status)
          (message "Fail to parse the `%s' `Device Enabled' status" name))
         (t status))))

    (defun xpointer--toggle-device (name)
      (let ((status (xpointer--device-enabled-p name)))
        (cond
         ((equal "0" status)
          (xpointer--set-device-enabled-to name "1"))
         ((equal "1" status)
          (xpointer--set-device-enabled-to name "0"))
         ((not status)
          (message "Fail to parse the `%s' `Device Enabled' status" name))
         (t status))))

    (defun xpointer-disable-mouse ()
      (interactive)
      (xpointer--disable-device xpointer-mouse-name))

    (defun xpointer-disable-touchpad ()
      (interactive)
      (xpointer--disable-device xpointer-touchpad-name))

    (defun xpointer-enable-mouse ()
      (interactive)
      (xpointer--enable-device xpointer-mouse-name))

    (defun xpointer-enable-touchpad ()
      (interactive)
      (xpointer--enable-device xpointer-touchpad-name))

    (defun xpointer-toggle-mouse ()
      (interactive)
      (xpointer--toggle-device xpointer-mouse-name))

    (defun xpointer-toggle-touchpad ()
      (interactive)
      (xpointer--toggle-device xpointer-touchpad-name))

    ;; Battery stuff
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

    ;; Reboot or shutdown
    (defcustom exwm-tear-down-background-color "DarkRed"
      "EXWM tear down background color."
      :type 'string
      :group 'display)

    (defcustom exwm-tear-down-hook nil
      "Hook to power-DOWN or re-BOOT the computer cleanly."
      :type 'hook
      :group 'exwm)

    (defun exwm-tear-down ()
      "Save all buffers and run `kill-emacs-hook' without killing exwm or Emacs."
      (save-some-buffers t)
      ;; `run-hooks' does not work with let binding.
      (setq exwm-tear-down-hook (thread-last kill-emacs-hook
                                  (remove 'exwm--server-stop)
                                  (remove 'server-force-stop)))
      (run-hooks 'exwm-tear-down-hook))

    (defun exwm-power-down ()
      "Save all Emacs buffers and power-DOWN the computer."
      (interactive)
      (buffer-face-set `(:background ,exwm-tear-down-background-color))
      (when (y-or-n-p "Really want to power-DOWN? ")
        (exwm-tear-down)
        (start-process-shell-command "power-DOWN" nil "sudo shutdown -h -t 2 now"))
      (buffer-face-mode -1))

    (defun exwm-re-boot ()
      "Save all Emacs buffers and re-BOOT the computer."
      (interactive)
      (buffer-face-set `(:background ,exwm-tear-down-background-color))
      (when (y-or-n-p "Really want to re-BOOT? ")
        (exwm-tear-down)
        (start-process-shell-command "re-BOOT" nil "sudo shutdown -r -t 2 now"))
      (buffer-face-mode -1))

    ;; User interface
    (defun exwm-alsamixer ()
      (interactive)
      (start-process-shell-command "alsamixer" nil "xterm -e alsamixer"))

    (defun exwm-invoke (command)
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))

    (defun exwm-lock-screen ()
      (interactive)
      (shell-command-to-string "i3lock -c 000000"))

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
    (xpointer-toggle-touchpad)
    (no-ac-display-battery-mode 1)
    (display-time-mode 1)
    (menu-bar-mode -1))

  (use-package exwm-floating
    :custom
    (exwm-floating-border-color "BlueViolet")
    (exwm-floating-border-width 3))

  (use-package exwm-input
    :custom
    ;; Bind `s-' prefix exwm specific keys when exwm gets enabled,
    ;; since those key-bindings may conflict with other window managers.
    (exwm-input-global-keys
     `(([?\s-&] . exwm-invoke)
       ([?\s-B] . exwm-re-boot)
       ([?\s-D] . exwm-power-down)
       ([?\s-a] . exwm-alsamixer)
       ([?\s-b] . exwm-workspace-switch-to-buffer)
       ([?\s-i] . exwm-invoke)
       ([?\s-k] . exwm-input-toggle-keyboard)
       ([?\s-l] . exwm-lock-screen)
       ([?\s-m] . xpointer-toggle-mouse)
       ([?\s-n] . next-window-any-frame)
       ([?\s-o] . other-window)
       ([?\s-p] . previous-window-any-frame)
       ([?\s-q] . window-toggle-side-windows)
       ([?\s-r] . exwm-reset)
       ([?\s-t] . xpointer-toggle-touchpad)
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
    (defun exwm-randr-connected-monitors ()
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (let* ((goal (rx (group (or "DP1" "HDMI1" "VIRTUAL1" "eDP1")) " connected"))
               (found (cl-loop with output = (buffer-string)
                               for line in (split-string output "\n")
                               when (string-match goal line)
                               collect (match-string 1 line))))
          found)))

    (defun on-exwm-randr-screen-change ()
      (let* ((monitors (exwm-randr-connected-monitors))
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
  ;; https://emacs.stackexchange.com/questions/10244/flycheck-could-not-find-file-under-load-path
  :preface
  (put 'flycheck-emacs-lisp-load-path 'safe-local-variable
       (lambda (p) (and (eq p 'inherit)
                        (string-equal (buffer-file-name) user-init-file))))
  :custom (flycheck-check-syntax-automatically (quote (idle-change newline save)))
  :hook ((emacs-lisp-mode
          python-mode) . flycheck-mode))

(use-package flymake
  :bind ((:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package flyspell
  :unless noninteractive
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

(use-package forge
  :commands
  (forge--url-equal))

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
               ("C-c C-o" . goto-address-at-point)))
  :hook
  ((compilation-mode
    eshell-mode
    shell-mode) . goto-address-mode)
  ((prog-mode) . goto-address-prog-mode))

(use-package git-commit
  :preface
  (put 'git-commit-major-mode 'safe-local-variable
       (let ((modes (quote (git-commit-mode
                            git-commit-elisp-text-mode
                            markdown-mode
                            org-mode
                            textmode))))
         (lambda (mode) (member mode modes))))
  :custom
  (git-commit-major-mode 'gfm-mode))

(use-package gpastel
  ;; Try to prevent gpaste-daemon from using 100 % cpu time by
  ;; disabling image support.
  :when (and (eq system-type 'gnu/linux) (not noninteractive))
  :commands (gpastel-mode)
  :init
  (when (zerop (call-process-shell-command
                "gsettings list-recursively org.gnome.GPaste"))
    (gpastel-mode)))

(use-package haskell-mode
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  :mode (((rx (seq ".cabal" eos)) . haskell-cabal-mode)
         ((rx (seq ".hs" (opt (or "-boot" "c")) eos)) . haskell-mode)
         ((rx (seq ".lhs" eos)) . haskell-literate-mode))
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
  ;; Works only with `helm-bookmark' and `helm-grep' out of the box.
  :unless noninteractive
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
  :custom
  (helm-M-x-reverse-history nil)
  :commands (helm-M-x)
  :init
  (when (eq use-helm-or-selectrum 'use-helm)
    (bind-keys :map global-map
               ("M-x" . helm-M-x))))

(use-package helm-config
  :unless noninteractive
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
               ("C-x p" . helm-browse-project)
               ("C-x r p" . helm-projects-history))))

(use-package helm-for-files
  :custom
  (helm-file-cache-fuzzy-match t)
  (helm-recentf-fuzzy-match t))

(use-package helm-grep
  ;; https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/
  :preface
  (defun !-helm--project-root ()
    "Return the project root directory or `default-directory'."
    (let ((root default-directory)
          (project (project-current)))
      (when project
        (setq root (cdr project)))
      root))

  (defun !-helm-rg (directory &optional with-types)
    "Grep for a string in DIRECTORY using rg.
WITH-TYPES, if non-nil, ask for file types to search in."
    (interactive "P")
    (require 'helm-adaptive)
    (helm-grep-ag-1 (expand-file-name directory)
                    (helm-aif (and with-types
                                   (helm-grep-ag-get-types))
                        (helm-comp-read
                         "RG type: " it
                         :must-match t
                         :marked-candidates t
                         :fc-transformer 'helm-adaptive-sort
                         :buffer "*helm rg types*"))))

  (defun !-helm-default-directory-search (&optional with-types)
    "Grep for a string in `default-directory' using rg.
WITH-TYPES, if non-nil, ask for file types to search in."
    (interactive "P")
    (!-helm-rg default-directory with-types))

  (defun !-helm-project-search (&optional with-types)
    "Grep for a string in current project using rg.
WITH-TYPES, if non-nil, ask for file types to search in."
    (interactive "P")
    (!-helm-rg (!-helm--project-root) with-types))

  (bind-keys :map global-map
             ("C-:" . !-helm-default-directory-search)
             ("C-!" . !-helm-project-search))
  :custom
  (helm-grep-ag-command (concat "rg"
                                " --color=never"
                                " --smart-case"
                                " --no-heading"
                                " --line-number %s %s %s"))
  :bind ((:map global-map ("M-g a" . helm-do-grep-ag)))
  :commands (helm-grep-ag-1
             helm-grep-ag-get-types))

(use-package helm-imenu
  :custom
  (helm-imenu-fuzzy-match t))

(use-package helm-locate
  :custom
  (helm-locate-fuzzy-match t))

(use-package helm-mode
  :unless noninteractive
  :commands (helm-comp-read
             helm-mode)
  :init
  (when (eq use-helm-or-selectrum 'use-helm)
    (helm-mode +1))
  :delight (helm-mode " 🎯"))

(use-package helm-net
  :custom
  (helm-net-prefer-curl (if (executable-find "curl") t nil)))

(use-package helm-occur
  :bind ((:map global-map ("M-s o" . helm-occur))))

(use-package helm-org
  :unless noninteractive
  :after helm-mode org
  :demand t
  :config
  (mapc (function (lambda (element)
                    (add-to-list 'helm-completing-read-handlers-alist element)))
        (quote ((org-capture . helm-org-completing-read-tags)
                (org-set-tags . helm-org-completing-read-tags)))))

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
  :unless noninteractive
  :custom
  (hydra-verbose t)
  :commands (hydra--call-interactively-remap-maybe
             hydra-add-font-lock
             hydra-default-pre
             hydra-keyboard-quit
             hydra-set-transient-map
             hydra-show-hint)
  :init
  (with-eval-after-load 'elfeed
    (bind-key
     "f"
     (defhydra hydra-elfeed-filter ()
       ("A" (elfeed-search-set-filter "@48-months-ago") "All"
        :column "A-Z")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("S" (elfeed-search-set-filter "@12-months-ago +*") "Starred")
       ("U" (elfeed-search-set-filter "@12-months-ago +unread") "Unread")
       ("ab" (elfeed-search-set-filter "@48-months-ago +howard") "abrams"
        :column "a-d")
       ("ac" (elfeed-search-set-filter "@12-months-ago +aclu") "aclu")
       ("b" (elfeed-search-set-filter "@12-months-ago +bof") "bof" )
       ("c" (elfeed-search-set-filter "@48-months-ago +chua") "chua")
       ("d" (elfeed-search-set-filter "@12-months-ago +dn") "dn")
       ("ef" (elfeed-search-set-filter "@12-months-ago +eff") "eff"
        :column "e-k")
       ("em" (elfeed-search-set-filter "@12-months-ago +emacsen") "emacsen")
       ("gs" (elfeed-search-set-filter "@48-months-ago +g-stein") "g-stein")
       ("i" (elfeed-search-set-filter "@12-months-ago +intercepted") "intercepted")
       ("ki" (elfeed-search-set-filter "@48-months-ago +kitchin") "kitchin")
       ("kr" (elfeed-search-set-filter "@48-months-ago +krehel") "krehel")
       ("l" (elfeed-search-set-filter "@12-months-ago +lqdn") "lqdn"
        :column "l-s")
       ("m" (elfeed-search-set-filter "@48-months-ago +maugham") "maugham")
       ("n" (elfeed-search-set-filter "@48-months-ago +neirhardt") "neirhardt")
       ("py" (elfeed-search-set-filter "@12-months-ago +python") "python")
       ("ps" (elfeed-search-set-filter "@48-months-ago +p-stavrou") "p-stavrou")
       ("s" (elfeed-search-set-filter "@48months-ago +science") "science")
       ("vs" (elfeed-search-set-filter "@48-months-ago +schneidermann") "schneidermann"
        :column "v-w")
       ("vx" (elfeed-search-set-filter "@48-months-ago +vxlabs") "vxlabs")
       ("w" (elfeed-search-set-filter "@48-months-ago +wellons") "wellons")
       ("*" (elfeed-search-toggle-all '*) "toggle *"
        :column "Other")
       ("C-g" nil "quit" :color blue))
     elfeed-search-mode-map))
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
  :config
  (hydra-add-font-lock))

(use-package ibuf-ext
  :after ibuffer
  :preface
  (defmacro make-ibuffer-saved-filter-group (group-name &rest filter-names)
    `(cons ,group-name
           (mapcar
            (lambda (filter-name)
              (assoc filter-name ibuffer-saved-filters))
            ',filter-names)))
  :custom
  (ibuffer-saved-filters
   '(("Code" (or (mode . python-mode)
                 (mode . shell-mode)))
     ("Dired" (mode . dired-mode))
     ("Doc" (or (mode . Info-mode)
                (mode . help-mode)
                (mode . helpful-mode)))
     ("EMMS" (or (mode . emms-lyrics-mode)
                 (mode . emms-mark-mode)
                 (mode . emms-playlist-mode)))
     ("EXWM" (mode . exwm-mode))
     ("Elisp" (mode . emacs-lisp-mode))
     ("Eshell" (mode . eshell-mode))
     ("Helm" (mode . helm-major-mode))
     ("Magit" (derived-mode . magit-mode))
     ("Org" (mode . org-mode))
     ("PDF" (mode . pdf-view-mode))
     ("Python" (mode . python-mode))
     ("Setup" (derived-mode . conf-mode))
     ("TeX" (or (derived-mode . tex-mode)
                (mode . bibtex-mode)))
     ("VC" (or (mode . vc-annotate-mode)
               (mode . vc-dir-mode)))))
  (ibuffer-saved-filter-groups
   (list
    (make-ibuffer-saved-filter-group
     "Emacs" "Elisp" "Doc" "Eshell" "Code" "Org" "TeX" "PDF"
     "Magit" "VC" "Dired" "Helm" "EMMS" "EXWM" "Setup")
    (make-ibuffer-saved-filter-group
     "Python" "Python" "Doc" "Eshell" "Code" "Org" "TeX" "PDF"
     "Magit" "VC" "Dired" "Helm" "EMMS" "EXWM" "Setup")
    (make-ibuffer-saved-filter-group
     "Text" "Org" "TeX" "PDF" "Doc" "Eshell" "Python" "Code"
     "Magit" "VC" "Dired" "Helm" "EMMS" "EXWM" "Setup")))
  :hook
  ((ibuffer-mode) . (lambda ()
                      (ibuffer-switch-to-saved-filter-groups
                       (caar ibuffer-saved-filter-groups))))
  :commands (ibuffer-switch-to-saved-filter-groups))

(use-package ibuffer
  ;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
  :unless noninteractive
  :custom
  (ibuffer-expert nil)
  (ibuffer-save-with-custom nil)
  (ibuffer-show-empty-filter-groups nil)
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
    (((class color) (background light)) :background "LightGrey"))))

(use-package keycast
  :custom
  (keycast-window-predicate 'keycast-active-frame-bottom-right-p))

(use-package langtool
  ;; https://github.com/marcanuy/dot-emacs/blob/master/.emacs
  :bind (:map ctl-x-4-map
              ("v" . langtool-check)
              ("V" . langtool-check-done)
              ("l" . langtool-switch-default-language)
              ("4" . langtool-show-message-at-point)
              ("g" . langtool-correct-buffer))
  :custom
  (langtool-bin (executable-find "languagetool"))
  (langtool-default-language "en-US"))

(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c e" . macrostep-expand)))
  :commands (macrostep-mode)
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
               ("C-x M-g" . magit-dispatch)))
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

(use-package magit-git
  ;; Info: (magit) MacOS Performance
  :custom
  (magit-git-executable (executable-find "git")))

(use-package magit-section
  :commands (magit-add-section-hook))

(use-package magit-status
  :bind ((:map global-map
               ("C-x g" . magit-status))))

(use-package magit-submodule
  :custom
  (magit-submodule-remove-trash-gitdirs t))

(use-package mailcap
  :if (eq system-type 'darwin)
  :commands (mailcap-add)
  :config
  (mailcap-add "application/pdf" #'pdf-view-mode #'window-system))

(use-package message
  ;; https://emacs.stackexchange.com/a/3653
  ;; Info: (emacs) Sending Mail
  :custom
  (message-sendmail-envelope-from 'header)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  :init
  (use-package sendmail
    :custom
    (mail-specify-envelope-from t)
    (mail-envelope-from 'header)
    (sendmail-program (executable-find "msmtp"))))

(use-package man
  :custom
  (Man-width 80))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package minibuffer
  :custom
  (completion-styles (quote (flex))))

(use-package native-complete
  ;; https://blog.binchen.org/posts/thoughts-on-native-shell-completion-in-emacs-emacsenautocompleteshell.html
  ;; https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/
  :after shell
  :commands (native-complete-setup-bash)
  :demand t
  :config
  (native-complete-setup-bash))

(use-package nov
  :mode ((rx (seq ".epub" eos)) . nov-mode))

(use-package novice
  ;; https://www.emacswiki.org/emacs/DisabledCommands
  :preface
  (defun enable-me (&rest _args)
    "Called when a disabled command is executed.
Enable it and re-execute it."
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

(use-package ob-jupyter
  :custom
  (org-babel-default-header-args:jupyter-python
   (quote ((:async . "yes")
           (:eval . "noexport")
           (:exports . "both")
           (:hlines . "no")
           (:kernel . "python3")
           (:session . "py")))))

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
  (defun !-org-active-current-time-stamp ()
    "Insert an active org-mode `current-time' timestamp."
    (interactive)
    (org-insert-time-stamp (current-time) 'with-hm))

  (defun !-org-inactive-current-time-stamp ()
    "Insert an inactive org-mode `current-time' timestamp."
    (interactive)
    (org-insert-time-stamp (current-time) 'with-hm 'inactive))

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
                       (mapconcat (function identity) paths "\n"))
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
  ;; See jupyter's README: jupyter must be the last item, since it
  ;; depends on other languages.
  (org-babel-load-languages (quote ((calc . t)
                                    (emacs-lisp . t)
                                    (eshell . t)
                                    (gnuplot . t)
                                    (latex . t)
                                    (lisp . t)
                                    (org . t)
                                    (python . t)
                                    (shell . t)
                                    (jupyter . t))))
  (org-catch-invisible-edits 'smart)
  (org-export-backends '(ascii beamer icalendar html md latex man odt org texinfo))
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . (lambda (path link)
                                        (message "Open %s" link)
                                        (eww-open-file path)))
                   ("\\.pdf\\'" . emacs)))
  (org-image-actual-width '(400))
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
  :commands (org-insert-time-stamp
             org-link-set-parameters
             org-narrow-to-block
             org-narrow-to-subtree)
  :init
  (require 'pyenv-mode))

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
  ;; https://github.com/sprig/org-capture-extension
  ;; https://gitlab.com/marcowahl/org-capture-button
  :preface
  (defun filter-org-link-description (description)
    "Replace square brackets with round brackets in DESCRIPTION."
    (replace-regexp-in-string
     "[][]"
     (lambda (match)
       (cond ((string= match "]") ")")
             ((string= match "[") "(")))
     description))
  :after org
  :custom
  (org-capture-templates
   (quote
    (("t" "Task" entry (file+headline "~/tmpfs/tasks.org" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("p" "Protocol" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
     ("L" "Protocol Link" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %?[[%:link][%(filter-org-link-description \"%:description\")]]\nCaptured On: %U")
     ("X" "Capture with org-capture-button" entry (file "~/tmpfs/notes.org")
      "* %:description%? :webcapture:
:PROPERTIES:\n:CAPTURE_DATE: %U\n:END:\n- see %a %l\n%i\n"
      :prepend t :empty-lines 1))))
  :demand t)

(use-package org-element
  :commands (org-element-map
             org-element-parse-buffer
             org-element-property))

(use-package org-mime
  :after org
  :commands (org-mime-change-element-style)
  :bind ((:map message-mode-map
               ("C-c M-o" . org-mime-htmlize))
         (:map org-mode-map
               ("C-c M-o" . org-mime-org-buffer-htmlize)))
  :config
  (setq org-mime-export-options
        (list :section-numbers nil
              :with-author nil
              :with-toc nil))
  (add-hook 'org-mime-html-hook
            (defun on-org-mime-html-hook-setup ()
              (org-mime-change-element-style
               "pre"
               "color:#E6E1DC; background-color:#232323; padding:0.5em;")
              (org-mime-change-element-style
               "blockquote"
               "border-left: 2px solid gray; padding-left: 4px;"))))

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

(use-package org-table
  :commands (orgtbl-mode))

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
  (org-export-allow-bind-keywords t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{})
  :commands (org-export-derived-backend-p
             org-export-read-attribute))

(use-package ox-extra
  :after ox
  :commands (ox-extras-activate)
  :demand t
  :config
  ;; This is a public API fix of `org-latex-header-blocks-filter', but
  ;; it is maybe not the fastest fix.  Another (less memory hungry and
  ;; therefore faster) fix is to use the (original) positions instead
  ;; of blocks and to steal code from `org-src--contents-area' to get
  ;; the begin/end positions of the export block contents.
  (defun org-latex-header-blocks-filter (backend)
    (when (org-export-derived-backend-p backend 'latex)
      (let ((blocks
	     (org-element-map (org-element-parse-buffer 'greater-element nil) 'export-block
	       (lambda (block)
	         (when (and (string= (org-element-property :type block) "LATEX")
			    (string= (org-export-read-attribute
				      :header block :header)
				     "yes"))
		   block)))))
        (mapc (lambda (block)
	        (goto-char (org-element-property :post-affiliated block))
                (let ((contents-lines (split-string
                                       (org-element-property :value block)
                                       "\n")))
                  (delete-region (org-element-property :begin block)
                                 (org-element-property :end block))
                  (dolist (line contents-lines)
                    (insert (concat "#+latex_header: "
                                    (replace-regexp-in-string "\\` *" "" line)
                                    "\n")))))
	      ;; go in reverse, to avoid wrecking the numeric positions
	      ;; earlier in the file
	      (reverse blocks)))))
  (ox-extras-activate '(ignore-headlines latex-header-blocks)))

(use-package ox-latex
  :after ox
  :custom
  (org-latex-caption-above nil)
  (org-latex-compiler "pdflatex")
  (org-latex-hyperref-template nil)
  (org-latex-logfiles-extensions '("blg" "lof" "log" "lot" "out" "toc"))
  (org-latex-pdf-process
   (mapconcat
    (function identity)
    (list "latexmk"
          "-pdflatex='pdflatex -shell-escape -interaction nonstopmode'"
          "-pdf -bibtex -f %f")
    " "))
  ;; Requires CUSTOM_ID property to suppress LaTeX section labels.
  (org-latex-prefer-user-labels t)
  :demand t
  :config
  (mapc (function (lambda (element)
                    (add-to-list 'org-latex-classes element)))
        (nreverse
         (quote (;; The postfixes +1, +2, +3, -1, -2, and -3 denote:
                 ;; +1 => [DEFAULT-PACKAGES]
                 ;; +2 => [PACKAGES]
                 ;; +3 => [EXTRA]
                 ;; -1 => [NO-DEFAULT-PACKAGES]
                 ;; -2 => [NO-PACKAGES]
                 ;; -3 => [NO-EXTRA]
                 ("elsarticle-1+2+3"	; Elsevier journals
                  "\\documentclass{elsarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                 ("article-1+2+3"
                  "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                 ("report-1+2+3"
                  "\\documentclass[11pt]{report}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                  ("\\part{%s}" . "\\part*{%s}")
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                 ("book-1+2+3"
                  "\\documentclass[11pt]{book}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                  ("\\part{%s}" . "\\part*{%s}")
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))))

(use-package pdf-tools
  :after pdf-view
  :custom
  (pdf-annot-activate-created-annotations t)
  :commands (pdf-tools-install)
  :demand t
  :config
  (pdf-tools-install t))

(use-package pdf-view
  :custom
  (pdf-view-display-size 'fit-page)
  :bind ((:map pdf-view-mode-map
               ("M-w" . pdf-view-kill-ring-save)))
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
  :unless noninteractive
  :custom (psession-save-buffers-unwanted-buffers-regexp
           (rx (or (seq "diary" eol)
                   (seq ".newsticker-cache" eol))))
  :commands (psession-mode
             psession-savehist-mode)
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
  (pyenv-mode-set "3.8.3"))

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
  (mapc (function (lambda (element)
                    (add-to-list 'recentf-exclude element)))
        (quote (no-littering-etc-directory
                no-littering-var-directory
                "/\\.git/.*\\'"
                "/\\.hg/.*\\'"
                "^/\\(?:ssh\\|su\\|sudo\\)?:"))))

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
  :unless noninteractive
  :commands (selectrum-mode)
  :init
  (when (eq use-helm-or-selectrum 'use-selectrum)
    (selectrum-mode +1)))

(use-package selectrum-prescient
  :after selectrum
  :commands (selectrum-prescient-mode)
  :init
  (selectrum-prescient-mode +1))

(use-package shell
  :custom
  (shell-file-name (executable-find "bash"))
  :bind ((:map shell-mode-map
               ("<tab>" . company-complete)))
  :hook
  ((shell-mode) . (lambda ()
                    (setq-local company-backends '((company-native-complete)))))
  :config
  (setenv "PAGER" "cat"))

(use-package shr
  :custom
  (shr-max-image-proportion 0.8)
  :commands (shr-browse-url))

(use-package simple
  :unless noninteractive
  :preface
  (defcustom overwrite-mode-background-color "DarkGreen"
    "Overwrite mode background color."
    :type 'string
    :group 'display)
  :custom
  (eval-expression-print-length nil)
  (kill-do-not-save-duplicates t)
  :commands (column-number-mode
             region-active-p)
  :hook
  ((help-mode
    text-mode) . visual-line-mode)
  :config
  (column-number-mode)
  (add-hook 'overwrite-mode-hook
            (defun on-overwrite-mode-toggle-background-color ()
              "Toggle background-color on overwrite-mode toggle."
              (if (bound-and-true-p overwrite-mode)
                  (buffer-face-set
                   `(:background ,overwrite-mode-background-color))
                (buffer-face-mode -1)))))

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
  ;; https://ebzzry.io/en/emacs-pairs/
  ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
  ;; https://github.com/ebzzry/dotfiles/blob/master/emacs/fkd/klavoj.el
  ;; http://xenodium.com/emacs-smartparens-auto-indent/index.html
  :unless noninteractive
  :preface
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
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
  ((ielm-mode
    prog-mode
    text-mode) . smartparens-mode)
  ((ielm-mode
    prog-mode) . smartparens-strict-mode)
  :commands (show-smartparens-global-mode
             sp-local-pair)
  :config
  (show-smartparens-global-mode +1)
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  :delight (smartparens-mode (" 🗘" (:eval (if smartparens-strict-mode "/s" "")))))

(use-package smartparens-config
  :after smartparens
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

(use-package syntactic-close
  ;; https://manuel-uberti.github.io/emacs/2017/09/17/syntactic-close/
  :bind ((:map global-map
               ("C-c C-c" . syntactic-close))))

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

(use-package vc-hg
  :custom
  (vc-hg-program (or (executable-find "chg")
                     (executable-find "hg"))))

(use-package view
  ;; https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84
  ;; https://www.reddit.com/r/emacs/comments/fojc1y/using_viewmode_for_modal_navigation/
  ;; https://www.youtube.com/watch?v=kZARKLxTeYQ
  :bind ((:map global-map
               ("M-g v" . view-mode))
         (:map view-mode-map
               ("n" . next-line)
               ("p" . previous-line)))
  :custom
  (view-read-only nil "Since C-x C-q fails to toggle both modes.")
  :config
  (add-hook 'view-mode-hook
            (defun on-view-mode-hook-change-cursor-type ()
              (setq cursor-type (if view-mode '(hbar . 3) 'box)))))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))

(use-package which-key
  :commands (which-key-mode)
  :delight (which-key-mode))

(use-package window
  :custom
  (display-buffer-alist
   (backquote
    ((,(rx "*Help" (zero-or-more nonl) "*" )
      (display-buffer-in-side-window)
      (window-width . 0.30)
      (side . left)
      (slot . -1))
     (,(rx (seq "*eshell" (zero-or-more nonl) "*"))
      (display-buffer-in-side-window)
      (window-height . 0.30)
      (side . left)
      (slot . 0))
     (,(rx "*Faces*")
      (display-buffer-in-side-window)
      (window-width . 0.30)
      (side . left)
      (slot . +1))))))

(use-package winner
  :bind ((:map winner-mode-map
               ("s-<right>" . winner-redo)
               ("s-<left>" . winner-undo)))
  :custom
  (winner-dont-bind-my-keys t))

(use-package with-editor
  :hook
  ((eshell-mode
    shell-mode) . with-editor-export-editor))

(use-package wordnut
  :bind* (("C-z C-w" . wordnut-search)))

(use-package writegood-mode
  :commands (writegood-mode))

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
            (defun on-after-init-hook-finalize ()
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
;; flycheck-emacs-lisp-load-path: inherit
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
