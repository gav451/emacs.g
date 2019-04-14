;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn                                  ; startup
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
  (setq cursor-type 'box)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 8)
  (setq tab-always-indent 'complete)
  ;; Darwin
  (when (boundp 'ns-alternate-modifier)
    (setq ns-alternate-modifier nil))
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))
  (when (boundp 'ns-right-command-modifier)
    (setq ns-right-command-modifier 'super))
  (when (eq window-system 'ns)
    (add-to-list 'initial-frame-alist '(height . 50))
    (add-to-list 'initial-frame-alist '(width . 170)))
  ;; Linux
  (when (getenv "EXWM")
    (menu-bar-mode 0))
  (when (string= (system-name) "venus")
    (display-battery-mode 1)))

(progn                                  ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn                                  ; `use-package' and `delight'
  (require 'use-package)
  (require 'delight)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0.001)
  (setq use-package-verbose t))

(use-package subr-x
  :config
  (put 'if-let   'byte-obsolete-info nil)
  (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer               nil)
  (auto-compile-mode-line-counter            t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest   t)
  (auto-compile-update-autoloads             t)
  :hook
  (auto-compile-inhibit-compile
   . auto-compile-inhibit-compile-detached-git-head)
  :commands
  auto-compile-on-load-mode
  auto-compile-on-save-mode
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package no-littering
  :commands
  no-littering-expand-etc-file-name
  :demand t)

(use-package epkg
  :custom
  (epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :commands
  custom-declare-variable
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
  :custom-face
  ;; C-h i: Elisp -> Display -> Faces -> Defining Faces.
  (aw-leading-char-face ((((class color) (background dark))
                          :background "gold" :foreground "purple3" :height 2.0)
                         (((class color) (background light))
                          :background "purple3" :foreground "gold" :height 2.0)
                         (t :foreground "gray100" :underline nil)))
  :commands
  ace-window-display-mode
  :bind* (("M-o" . ace-window))
  :after avy
  :init
  (ace-window-display-mode))

(use-package alert
  :custom
  (alert-default-style 'libnotify))

(use-package autorevert
  :custom
  (auto-revert-mode-text " ⏎"))

(use-package avy
  :custom
  (avy-all-windows t)
  :commands
  avy-setup-default
  :bind* (("C-:" . avy-goto-word-1))
  :init
  (avy-setup-default))

(use-package bibtex
  :after tex
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path '("~/VCS/research/papers"))
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org")
  (bibtex-user-optional-fields
   '(("abstract")
     ("doi" "Digital Object Identifier")
     ("url" "Universal Ressource Locator"))))

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
     (".*reddit.com" . browse-url-generic)
     (".*wikipedia.*" . browse-url-generic)
     (".*youtube.*" . browse-url-generic)
     ("." . eww-browse-url)))
  (browse-url-generic-program (or (executable-find "qutebrowser")
                                  (executable-find "firefox")))
  :commands
  browse-url
  browse-url-generic)

(use-package company
  ;; https://emacs.stackexchange.com/questions/9835/how-can-i-prevent-company-mode-completing-numbers
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
              ("0" . my-company-complete-number)
              ("1" . my-company-complete-number)
              ("2" . my-company-complete-number)
              ("3" . my-company-complete-number)
              ("4" . my-company-complete-number)
              ("5" . my-company-complete-number)
              ("6" . my-company-complete-number)
              ("7" . my-company-complete-number)
              ("8" . my-company-complete-number)
              ("9" . my-company-complete-number)
              ("<space>" . my-company-insert-abort)
              ("C-y" . yas-expand-from-trigger-key))
  :hook
  ((LaTeX-mode emacs-lisp-mode org-mode) . company-mode)
  :delight company-mode " 𝍎")

(use-package counsel
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (concat
                                    ;; file names beginning with # or .
                                    "\\(?:\\`[#\\.]\\)"
                                    ;; file names ending with # or ~
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"))
  (counsel-grep-swiper-limit (lsh 1 20))
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-command-only)
  :commands
  counsel-describe-face
  counsel-describe-function
  counsel-describe-variable
  counsel-info-lookup-symbol
  counsel-linux-app-format-function-command-only
  :bind (;; Allow shadowing in `pdf-view-mode-map' by use of
         ;; "C-r" instead of [remap isearch-backward] and
         ;; "C-s" instead of [remap isearch-forward].
         ("C-r" . counsel-grep-or-swiper)
         ("C-s" . counsel-grep-or-swiper)
         ;; Avoid shadowing `eshell-forward-argument'.
         ("C-c C-f" . counsel-recentf))
  :bind* (("C-x C-f" . counsel-find-file)
          ("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-c C-g" . counsel-rg)
          ("C-c u" . counsel-unicode-char)))

(use-package cython-mode
  :mode "\\.py[xdi]\\'")

(use-package dash
  :commands
  dash-enable-font-lock
  :config
  (dash-enable-font-lock))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  :commands
  global-diff-hl-mode
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package dired
  :no-require t
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
  (wdired-allow-to-change-permissions t)
  :bind (:map dired-mode-map
              ("E" . my-dired-eww-find-file)
              ("M-s y" . my-dired-rsync))
  :hook
  (dired-mode . auto-revert-mode)
  :commands
  dired-get-file-for-visit
  dired-get-marked-files)

(use-package dired-aux
  :after dired
  :commands
  dired-dwim-target-directory)

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
      (extension "gif" "jpg" "png" "tiff"))
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
  :bind (:map dired-mode-map
              ("M-s g" . dired-filter-group-mode)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("M-s n" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("M-s i" . dired-subtree-insert)
              ("M-s r" . dired-subtree-remove)))

(use-package dired-x
  :after dired)

(use-package display-line-numbers
  :no-require t
  :custom-face
  (line-number-current-line ((t (:inherit highlight))))
  :hook
  ((LaTeX-mode
    org-mode
    prog-mode) . display-line-numbers-mode))

(use-package easy-kill
  ;; https://emacsredux.com/blog/2018/11/09/an-easy-kill/
  ;; https://emacsredux.com/blog/2019/01/10/the-emacs-year-in-review/
  :bind (;; Allow shadowing in `pdf-view-mode-map' by use of
         ;; "M-w" instead of [remap kill-ring-save].
         ("M-w" . easy-kill)))

(use-package elec-pair
  :hook
  ((emacs-lisp-mode
    eshell-mode
    org-mode
    python-mode) . electric-pair-local-mode))

(use-package electric-operator
  :hook
  ((python-mode) . electric-operator-mode)
  :delight electric-operator-mode " ⌤")

(use-package elfeed
  ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  :preface
  (defun my-elfeed-db-load-and-open ()
    "Wrapper to load the elfeed database before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  (defun my-elfeed-save-db-and-quit ()
    (interactive)
    (elfeed-db-save)
    (quit-window))
  (defun my-elfeed-show-visit-external ()
    "Wrapper to visit the current entry using an external browser."
    (interactive)
    (let ((browse-url-generic-program (or (executable-find "qutebrowser")
                                          (executable-find "firefox"))))
      (elfeed-show-visit t)))
  (defun my-elfeed-show-shr-browse-url-external ()
    "Wrapper to browse the URL under point using an external browser."
    (interactive)
    (let ((shr-external-browser #'browse-url-generic))
      (shr-browse-url t)))
  (defun my-elfeed-toggle-star ()
    "Wrapper to toggle all starred elfeed search entries."
    (interactive)
    (elfeed-search-toggle-all '*))
  :custom
  (elfeed-db-directory "~/SYNC/elfeed/db")
  (elfeed-feeds
   '(("http://emacshorrors.com/feed.atom" schneidermann)
     ("http://emacsninja.com/feed.atom" schneidermann)
     ("http://www.howardism.org/index.xml" howard)
     ("http://nullprogram.com/feed/" wellons)
     ;; https://www.emacswiki.org/emacs/PlanetEmacsen
     ("http://planet.emacslife.com/atom.xml" emacsen)
     ("http://pragmaticemacs.com/feed/" maugham)
     ("http://sachachua.com/blog/category/emacs/feed" chua)
     ("https://act.eff.org/action.atom" eff)
     ("https://feeds.feedburner.com/InterceptedWithJeremyScahill" intercepted)
     ("https://www.aclu.org/taxonomy/feed-term/2152/feed" aclu)
     ("https://www.bof.nl/rss/" bof)
     ("https://www.democracynow.org/podcast-video.xml" dn)
     ("https://www.laquadrature.net/fr/rss.xml" lqdn)))
  (elfeed-enclosure-default-dir (expand-file-name "~/tmpfs/"))
  :bind* (("C-x w" . my-elfeed-db-load-and-open))
  :bind (:map elfeed-search-mode-map
              ("q" . my-elfeed-save-db-and-quit))
  :commands
  elfeed
  elfeed-db-load
  elfeed-db-save
  elfeed-search-set-filter
  elfeed-search-toggle-all
  elfeed-search-update--force
  elfeed-show-visit
  :config
  (make-directory elfeed-db-directory t)
  (bind-keys :map elfeed-show-mode-map
             ("B" . my-elfeed-show-visit-external)
             ("&" . my-elfeed-show-shr-browse-url-external))
  (bind-key
   "j"
   (defhydra hydra-elfeed-filter ()
     ""
     ("A" (elfeed-search-set-filter "@6-months-ago") "All"
      :column "A-Z")
     ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
     ("S" (elfeed-search-set-filter "@6-months-ago +*") "Starred")
     ("U" (elfeed-search-set-filter "@6-months-ago +unread") "Unread")
     ("ab" (elfeed-search-set-filter "@6-months-ago +abrams") "abrams"
      :column "a-c")
     ("ac" (elfeed-search-set-filter "@6-months-ago +aclu") "aclu")
     ("b" (elfeed-search-set-filter "@6-months-ago +bof") "bof" )
     ("c" (elfeed-search-set-filter "@6-months-ago +chua") "chua")
     ("d" (elfeed-search-set-filter "@6-months-ago +dn") "dn"
      :column "d-i")
     ("e" (elfeed-search-set-filter "@6-months-ago +emacsen") "emacsen")
     ("f" (elfeed-search-set-filter "@6-months-ago +eff") "eff")
     ("i" (elfeed-search-set-filter "@6-months-ago +intercepted") "intercepted")
     ("l" (elfeed-search-set-filter "@6-months-ago +lqdn") "lqdn"
      :column "i-z")
     ("m" (elfeed-search-set-filter "@6-months-ago +maugham") "maugham")
     ("s" (elfeed-search-set-filter "@6-months-ago +schneidermann") "schneidermann")
     ("w" (elfeed-search-set-filter "@6-months-ago +wellons") "wellons")
     ("*" my-elfeed-toggle-star "toggle *"
      :column "Other")
     ("C-g" nil "quit" :color blue))
   elfeed-search-mode-map))

(use-package emms
  ;; Let mpd handle most sound, and mpv everything else (ideally video only).
  :custom
  (emms-player-list '(emms-player-mpd emms-player-mpv))
  :commands
  emms-player-set)

(use-package emms-browser
  :commands
  emms-smart-browse)

(use-package emms-info
  :custom
  (emms-info-functions nil))

(use-package emms-info-libtag
  :when (executable-find "emms-print-metadata")
  :after emms-setup
  :commands
  emms-info-libtag
  :config
  (add-to-list 'emms-info-functions 'emms-info-libtag))

(use-package emms-player-mpd
  ;; Let mpd most (ideally all) sound.
  ;; BUG: mpd fails to handle the NPO m3u files, and I fail to work around it.
  :after emms-setup
  :custom
  (emms-player-mpd-music-directory "/home/gav/Music")
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6600")
  :commands
  emms-info-mpd
  :config
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (emms-player-set emms-player-mpd 'regex
                   (apply #'emms-player-simple-regexp
                          '("flac" "m3u" "mp3" "ogg" "opus" "pls" "soundcloud"))))

(use-package emms-player-mpv
  :after emms-setup)

(use-package emms-player-simple
  :commands
  emms-player-simple-regexp)

(use-package emms-playlist-mode
  :custom
  (emms-playlist-mode-center-when-go t)
  :commands
  emms
  emms-playlist-mode-go)

(use-package emms-setup
  :commands
  emms-all
  :defer 10
  :config
  (emms-all))

(use-package emms-streams
  :custom
  ;; To show the current playlist, do either
  ;; "M-x emms" or "M-x emms-playlist-mode-go".
  (emms-stream-bookmarks-file (no-littering-expand-etc-file-name "emms/streams"))
  (emms-stream-default-action "play")
  :commands
  emms-streams)

(use-package emms-volume
  :commands
  emms-volume-mode-minus
  emms-volume-mode-plus)

(use-package engine-mode
  ;; https://github.com/asok/.emacs.d/blob/master/inits/init-engine-mode.el
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-engine-mode.el
  ;; https://gitlab.com/ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-engine.el
  :commands
  engine-mode
  engine/execute-search
  engine/get-query
  :defer 10
  :config
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
  (bind-key*
   "C-z C-e"
   (defhydra hydra-engine (:color blue)
     "Search"
     ("a" engine/search-arch-wiki      "arch-wiki")
     ("d" engine/search-duck-duck-go   "duck-duck-go")
     ("g" engine/search-github         "github")
     ("o" engine/search-open-streetmap "open-streetmap")
     ("w" engine/search-wikipedia      "wikipedia"))))

(use-package epa
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (when (eq system-type 'darwin)
    (setq epg-gpg-program "gpg2")))

(use-package eshell
  ;; http://emacshorrors.com/post/life-is-too-much
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  ;; https://github.com/wasamasa/dotemacs/blob/master/init.org#eshell
  :preface
  (defun my-eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp)
             (looking-back
              eshell-prompt-regexp (min (- (point) (length (eval eshell-banner-message)))
                                        (lsh 1 16))))
        (eshell-life-is-too-much)
      (delete-char arg)))

  (defun on-eshell-mode ()
    (bind-keys :map eshell-mode-map
               ("C-d" . my-eshell-quit-or-delete-char)))
  :custom
  (eshell-aliases-file (no-littering-expand-etc-file-name "eshell/alias"))
  (eshell-hist-ignoredups t)
  (eshell-ls-initial-args nil)
  (eshell-save-history-on-exit t)
  (eshell-visual-commands '("htop" "ipython" "jupyter" "less" "ncftp" "python" "tmux"))
  :hook
  (eshell-mode . on-eshell-mode)
  :defines
  eshell-banner-message
  eshell-prompt-regexp
  :commands
  eshell-life-is-too-much)

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
  :commands
  eww-browse-url
  eww-current-url
  eww-open-filed
  eww-readable)

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_AGENT_INFO"))
  :commands
  exec-path-from-shell-initialize
  :init
  (exec-path-from-shell-initialize)
  :demand t)

(use-package exwm
  ;; https://github.com/DamienCassou/emacs.d/blob/master/init.el
  ;; https://github.com/ch11ng/exwm/wiki
  ;; https://github.com/dakra/dmacs/blob/master/init.org
  ;; https://github.com/technomancy/dotfiles/blob/master/.emacs.d/phil/wm.el
  ;; https://gitlab.com/ambrevar/dotfiles/tree/master/.emacs.d
  :preface
  (defcustom my-exwm-teardown-hook nil
    "Hook to power-DOWN or re-BOOT the computer cleanly."
    :type 'hook
    :group 'exwm)
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
    (buffer-face-set '(:background "DarkRed"))
    (when (y-or-n-p "Really want to power-DOWN?")
      (my-exwm-teardown)
      (start-process-shell-command "power-DOWN" nil "sudo shutdown -h -t 2 now"))
    (buffer-face-mode -1))

  (defun my-exwm-re-boot ()
    "Save all Emacs buffers and re-BOOT the computer."
    (interactive)
    (buffer-face-set '(:background "DarkRed"))
    (when (y-or-n-p "Really want to re-BOOT?")
      (my-exwm-teardown)
      (start-process-shell-command "re-BOOT" nil "sudo shutdown -r -t 2 now"))
    (buffer-face-mode -1))

  (defun on-exwm-update-class ()
    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                (string= "gimp" exwm-instance-name))
      (exwm-workspace-rename-buffer exwm-class-name)))

  (defun on-exwm-update-title ()
    (when (or (not exwm-instance-name)
              (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string= "gimp" exwm-instance-name))
      (exwm-workspace-rename-buffer exwm-title)))

  :when (getenv "EXWM")
  :custom
  (display-time-string-forms '((format-time-string " %F %R")))
  (exwm-floating-border-color "BlueViolet")
  (exwm-floating-border-width 3)
  ;; Bind `s-' prefix exwm specific keys when exwm gets enabled,
  ;; since those key-bindings may conflict with other window managers.
  (exwm-input-global-keys
   `(([?\s-&] . my-exwm-invoke)
     ([?\s-b] . my-exwm-re-boot)
     ([?\s-d] . my-exwm-power-down)
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
  (exwm-layout-show-all-buffers t)
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
      floating-mode-line nil)))

  (exwm-workspace-number 2)
  (exwm-workspace-show-all-buffers t)
  :hook
  (exwm-update-class . on-exwm-update-class)
  (exwm-update-title . on-exwm-update-title)
  :commands
  exwm-enable
  exwm-input-set-key
  exwm-input-toggle-keyboard
  exwm-reset
  exwm-workspace-rename-buffer
  exwm-workspace-switch
  exwm-workspace-switch-create
  :init
  (exwm-enable)
  :config
  (display-time-mode 1))

(use-package exwm-edit
  :preface
  (defun on-exwm-edit-compose ()
    (funcall 'org-mode))
  :after exwm
  :hook
  (exwm-edit-compose . on-exwm-edit-compose)
  :commands
  exwm-edit--compose
  :init
  (exwm-input-set-key (kbd "C-c '") #'exwm-edit--compose)
  (exwm-input-set-key (kbd "C-c C-'") #'exwm-edit--compose))

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
           (wop)
           (command
            (cond
             ((eq count 2)
              (dotimes (i 10)
                (if (cl-evenp i)
                    (setq wop (plist-put wop i (car monitors)))
                  (setq wop (plist-put wop i (cadr monitors)))))
              (message "Exwm-randr: 2 monitors")
              (format "xrandr --output %s --auto --above %s"
                      (cadr monitors) (car monitors)))
             ((eq count 1)
              (dotimes (i 10)
                (setq wop (plist-put wop i (car monitors))))
              (message "Exwm-randr: 1 monitor")
              "xrandr"))))
      (setq exwm-randr-workspace-monitor-plist wop)
      (start-process-shell-command "xrandr" nil command)))

  :after exwm
  :when (string= (system-name) "venus")
  :hook
  (exwm-randr-screen-change . on-exwm-randr-screen-change)
  :commands
  exwm-randr-enable
  :init
  (exwm-randr-enable))

(use-package face-remap
  :commands
  buffer-face-mode
  buffer-face-set
  :delight buffer-face-mode)

(use-package faces
  :when window-system
  :commands invert-face
  :init
  (invert-face 'default))

(use-package files
  :commands
  executable-find
  :custom
  (insert-directory-program (or (executable-find "gls")
                                (executable-find "ls"))))

(use-package flycheck
  :commands
  flycheck-mode)

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package free-keys
  :custom
  (free-keys-modifiers '("" "C" "M" "C-M" "s"))
  :commands
  free-keys)

(use-package god-mode
  :preface
  (defcustom god-local-mode-background-color "DarkBlue"
    "God-Local mode background color."
    :type 'string
    :group 'god)
  (defcustom overwrite-mode-background-color "DarkGreen"
    "Overwrite mode background color."
    :type 'string
    :group 'editing-basics)
  (defun on-overwrite-god-mode-toggle ()
    "Toggle god-mode on overwrite-mode"
    (if (bound-and-true-p overwrite-mode)
        (progn
          (god-local-mode-pause)
          (buffer-face-set `(:background ,overwrite-mode-background-color)))
      (progn
        (god-local-mode-resume)
        (buffer-face-mode -1))))
  (defun on-god-local-mode-toggled ()
    (if god-local-mode
        (buffer-face-set `(:background ,god-local-mode-background-color))
      (buffer-face-mode -1)))
  :bind (("<escape>" . god-local-mode))
  :commands
  god-local-mode-pause
  god-local-mode-resume
  :hook
  ((god-mode-disabled god-mode-enabled) . on-god-local-mode-toggled)
  (overwrite-mode . on-overwrite-god-mode-toggle)
  :delight god-local-mode " 🌪")

(use-package goto-addr
  ;; https://xenodium.com/#actionable-urls-in-emacs-buffers
  :bind (:map goto-address-highlight-keymap
              ("C-!" . goto-address-at-point))
  :hook
  ((emacs-lisp-mode
    eshell-mode
    prog-mode
    shell-mode) . goto-address-mode))

(use-package gpastel
  :when (eq system-type 'gnu/linux)
  :commands
  gpastel-mode
  :defer 10
  :config
  (when (= 0 (call-process-shell-command
              "gsettings list-recursively org.gnome.GPaste"))
    (gpastel-mode)))

(use-package help
  :no-require t
  :bind (:map help-map
              ("A" . helpful-at-point)
              ("C" . helpful-command)
              ("F" . helpful-function)
              ("M" . helpful-macro)
              ("S" . counsel-info-lookup-symbol)
              ("f" . counsel-describe-function)
              ("k" . helpful-key)
              ("v" . counsel-describe-variable)
              ("z" . describe-minor-mode))
  :commands
  temp-buffer-resize-mode
  :init
  (temp-buffer-resize-mode 1))

(use-package helpful
  :commands
  helpful-at-point
  helpful-callable
  helpful-command
  helpful-function
  helpful-key
  helpful-macro
  helpful-variable)

(use-package hl-line
  :commands
  global-hl-line-mode
  hl-line-mode
  :hook
  ((elfeed-show-mode
    emms-playlist-mode
    emms-stream
    help-mode) . hl-line-mode))

(use-package hydra
  ;; http://oremacs.com/2016/04/04/hydra-doc-syntax/
  :preface
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
_d_  ?d? display-line-numbers  _l_  ?l? lispy            _wg_ ?wg? writegood
_fc_ ?fc? flycheck              _o_  ?o? org-table        _wk_ ?wk? which-key
_fl_ ?fl? font-lock             _p_  ?p? electric-pair    _ws_ ?ws? white-space
_fs_ ?fs? flyspell              _r_  ?r? read-only
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
     ("fs" #'flyspell-mode
      (if (bound-and-true-p flyspell-mode) "[X]" "[ ]"))
     ("g" #'goto-address-mode
      (if (bound-and-true-p goto-address-mode) "[X]" "[ ]"))
     ("ii" #'iimage-mode
      (if (bound-and-true-p iimage-mode) "[X]" "[ ]"))
     ("it" (setq indent-tabs-mode (not (bound-and-true-p indent-tabs-mode)))
      (if (bound-and-true-p indent-tabs-mode) "[X]" "[ ]"))
     ("l" #'lispy-mode
      (if (bound-and-true-p lispy-mode) "[X]" "[ ]"))
     ("o" #'orgtbl-mode
      (if (bound-and-true-p orgtbl-mode) "[X]" "[ ]"))
     ("p" #'electric-pair-local-mode
      (if (bound-and-true-p electric-pair-mode) "[X]" "[ ]"))
     ("r" #'read-only-mode
      (if (bound-and-true-p buffer-read-only) "[X]" "[ ]"))
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
  :commands
  hydra--call-interactively-remap-maybe
  hydra-default-pre
  hydra-keyboard-quit
  hydra-set-transient-map
  hydra-show-hint)

(use-package iedit
  :custom
  (iedit-toggle-key-default nil)
  :bind
  (:map global-map ("C-;" . iedit-mode))
  (:map isearch-mode-map ("C-;" . iedit-mode-from-isearch))
  (:map esc-map ("C-;" . iedit-execute-last-modification))
  (:map help-map ("C-;" . iedit-mode-toggle-on-function)))

(use-package ivy
  ;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
  ;; https://github.com/sam217pa/emacs-config
  :custom
  (ivy-case-fold-search-default 'auto)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-use-ignore-default t)
  (ivy-use-virtual-buffers t)
  :bind* (("C-c C-r" . ivy-resume))
  :commands
  ivy-completing-read
  ivy-mode
  ivy-read
  :demand t
  :config (ivy-mode)
  :delight ivy-mode " 𝝓")

(use-package jupyter
  :commands jupyter-run-repl)

(use-package latex
  :load tex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (LaTeX-electric-left-right-brace t)
  :commands
  TeX-latex-mode
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . turn-on-reftex))

(use-package lentic
  :preface
  :after org
  :commands
  global-lentic-mode
  :defer 10
  :config
  (global-lentic-mode 1)
  :delight lentic-mode " 🎥")

(use-package lispy
  :custom
  (lispy-compat '(edebug god-mode macrostep))
  :commands
  lispy-mode
  :hook
  ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . lispy-mode)
  :delight lispy-mode " 🗘")

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand))
  :bind (:map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand))
  :config
  (use-package use-package))

(use-package magit
  ;; https://stackoverflow.com/questions/4114095/how-to-revert-a-git-repository-to-a-previous-commit
  ;; https://stackoverflow.com/questions/9529078/how-do-i-use-git-reset-hard-head-to-revert-to-a-previous-commit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :commands
  magit-add-section-hook
  :config
  ;; See magit's info on section hooks: use `magit-add-section-hook'
  ;; instead of `add-hooks', `:custom', or `defcustom'.
  ;; See also: https://github.com/magit/magit/issues/2657.
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream))

(use-package mailcap
  :if (eq system-type 'darwin)
  :commands mailcap-add
  :config
  (mailcap-add "application/pdf" #'pdf-view-mode #'window-system))

(use-package man
  :config (setq Man-width 80))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package multi-term
  :custom
  (multi-term-programe (executable-find "zsh"))
  :commands
  multi-term)

(use-package navi-mode
  :after outshine
  :bind (("M-s n" . navi-search-and-switch)
         ("M-s s" . navi-switch-to-twin-buffer)
         ("M-s M-s" . navi-switch-to-twin-buffer)))

(use-package ob-async
  ;; Demand loading of this package after loading org-mode.
  :after org
  :demand t
  :custom
  (ob-async-no-async-languages-alist '("jupyter-julia" "jupyter-python")))

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

  ;; https://stackoverflow.com/questions/10274642/emacs-completion-at-point-functions
  ;; https://emacs.stackexchange.com/questions/15276/how-do-i-write-a-simple-completion-at-point-functions-function
  ;; https://oremacs.com/2017/10/04/completion-at-point
  (defun org-get-verbatims ()
    (let (result)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([A-Za-z]+\\)=" nil t)
            (cl-pushnew
             (match-string-no-properties 0) result :test 'equal))
          result))))

  (defun capf-org-verbatim ()
    (when (looking-back "=[A-Za-z]+" 128)
      (let ((candidates (org-get-verbatims)))
        (when candidates
          (list (match-beginning 0)
                (match-end 0)
                candidates)))))

  (defun on-org-mode-hook ()
    (setq completion-at-point-functions
          '(capf-org-verbatim
            t))
    ;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
    (setq-local electric-pair-inhibit-predicate
                (lambda (c)
                  (if (and (char-equal c ?<)
                           (char-equal (char-before (1- (point))) ?\n))
                      t
                    (funcall (default-value 'electric-pair-inhibit-predicate) c)))))
  :custom
  (org-adapt-indentation nil)
  (org-agenda-exporter-settings '((ps-landscape-mode t)
                                  (ps-number-of-columns 2)
                                  (ps-paper-type 'a4)
                                  (ps-print-color-p nil)
                                  (ps-print-header nil)))
  (org-agenda-files '("~/VCS/pim/jobs.org"))
  (org-agenda-span 70)
  (org-babel-python-command "python -E")
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/tmpfs/tasks.org" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("p" "Protocol" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (org-catch-invisible-edits 'show-and-error)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0 "Preserve Python code block indentation.")
  (org-export-backends '(ascii beamer icalendar html md latex man odt org texinfo))
  (org-export-with-sub-superscripts '{})
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . (lambda (path link)
                                        (message "Open %s" link)
                                        (eww-open-file path)))
                   ("\\.pdf\\'" . emacs)))
  (org-latex-caption-above nil)
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc"  t ("pdflatex"))
     ("T1"   "fontenc"   t ("pdflatex"))
     (""     "graphicx"  t)
     (""     "grffile"   t)
     (""     "longtable" nil)
     (""     "wrapfig"   nil)
     (""     "rotating"  nil)
     ("normalem" "ulem"  t)
     (""     "amsmath"   t)
     (""     "textcomp"  t)
     (""     "amssymb"   t)
     (""     "capt-of"   nil)
     ("hyperfootnotes=false" "hyperref"  nil)))
  (org-latex-compiler "lualatex")
  (org-latex-hyperref-template nil)
  (org-latex-logfiles-extensions '("blg" "lof" "log" "lot" "out" "toc"))
  (org-latex-pdf-process
   '("lualatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b.aux"
     "lualatex -interaction nonstopmode -output-directory %o %f"
     "lualatex -interaction nonstopmode -output-directory %o %f"))
  ;; Requires CUSTOM_ID property to suppress LaTeX section labels.
  (org-latex-prefer-user-labels t)
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
     ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))
  (org-modules
   '(org-bibtex
     org-eww
     org-info
     org-protocol))
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t "Preserve Python code block indentation.")
  (org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "DEFERRED" "ZAPPED"))))
  (org-use-sub-superscripts '{})
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c l"   . org-store-link)
         ("C-c C-l" . org-insert-link-global))
  :mode
  ("\\.org\\'" . org-mode)
  :hook
  (org-mode . on-org-mode-eval-blocks)
  (org-mode . on-org-mode-hook)
  :commands
  org-babel-do-load-languages
  org-link-set-parameters
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((calc . t)
                                 (ditaa . t)
                                 (dot . t)
                                 (emacs-lisp . t)
                                 (gnuplot . t)
                                 (latex . t)
                                 (org . t)
                                 (python . t)
                                 (shell . t)))
  ;; Short-circuit org-ref-link-set-paramaters in org-ref-utils:
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

(use-package org-element
  :functions
  org-element-map
  org-element-parse-buffer
  org-element-property)

(use-package org-protocol-capture-html
  ;; https://www.reddit.com/r/emacs/comments/9ze1ln/capture_orgmode_bookmarks_from_qutebrowser_with/
  ;; Demand loading of this package after loading org-mode.
  :after org
  :demand t)

(use-package org-ref
  ;; Demand loading of this package after loading org-mode.
  :after org
  :demand t
  :custom
  (org-ref-bibliography-notes "~/VCS/research/notes/notes.org")
  (org-ref-cite-color "LawnGreen")
  (org-ref-ref-color "OrangeRed")
  (org-ref-label-color "DeepPink")
  (org-ref-completion-library 'org-ref-ivy-cite)
  (org-ref-default-bibliography '("~/VCS/research/refs.bib"))
  (org-ref-pdf-directory '("~/VCS/research/papers")))

(use-package org-ref-core
  :commands
  org-ref-get-labels)

(use-package org-ref-glossary
  :commands
  or-follow-glossary)

(use-package outline
  :custom
  (outline-minor-mode-prefix "\M-#")
  :delight outline-minor-mode " ✎")

(use-package outshine
  :after outline
  :commands
  outshine-mode
  :hook
  ((emacs-lisp-mode) . outshine-mode)
  :defer 5
  :delight outshine-mode " 🌅")

(use-package paren
  :commands
  show-paren-mode
  :init
  (show-paren-mode))

(use-package pdf-tools
  :custom
  (pdf-annot-activate-created-annotations t)
  :commands
  pdf-tools-install
  :magic
  ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t)
  (bind-keys :map pdf-view-mode-map
             ("C-r" . isearch-backward)
             ("C-s" . isearch-forward)
             ("M-w" . pdf-view-kill-ring-save)))

(use-package pdf-view
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-imagemagick t)
  :commands
  pdf-view-kill-ring-save)

(use-package peep-dired
  :after dired
  :custom (peep-dired-cleanup-on-disable t)
  :bind (:map dired-mode-map
              ("M-s p" . peep-dired)))

(use-package python
  ;; https://github.com/davidhalter/jedi/issues/1085
  ;; https://github.com/jorgenschaefer/elpy/issues/1115
  ;; https://github.com/jorgenschaefer/elpy/issues/1123
  ;; https://github.com/jorgenschaefer/elpy/pull/1279
  :custom
  (python-shell-interpreter-args "-E -i")
  :interpreter ("python" . python-mode)
  :mode ("\\.pyw?\\'" . python-mode)
  :config
  (use-package elpy
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
    (elpy-company-post-completion-function #'elpy-company-post-complete-parens)
    (elpy-modules '(elpy-module-sane-defaults
                    elpy-module-company
                    elpy-module-eldoc
                    elpy-module-flymake
                    elpy-module-pyvenv))
    (elpy-remove-modeline-lighter nil)
    (elpy-rpc-ignored-buffer-size (lsh 1 18))
    :commands
    elpy-company-post-complete-parens
    elpy-enable
    elpy-rpc
    elpy-rpc--buffer-contents
    :init
    (elpy-enable)
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
                  success error)))))

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

(use-package reftex
  :after tex
  :custom
  (reftex-default-bibliography "~/VCS/research/refs.bib")
  :commands
  turn-on-reftex
  :delight reftex-mode " 📑")

(use-package reveal
  :hook
  (emacs-lisp-mode . reveal-mode)
  :delight " 👀")

(use-package savehist
  :commands
  savehist-mode
  :config (savehist-mode))

(use-package saveplace
  :commands
  save-place-mode
  :config (save-place-mode))

(use-package shr
  :commands
  shr-browse-url)

(use-package simple
  :commands
  column-number-mode
  :config (column-number-mode))

(use-package swiper
  :custom
  (swiper-action-recenter t))

(use-package synosaurus
  :custom
  (synosaurus-choose-method 'default)
  :bind* (("C-z C-s l" . synosaurus-lookup)
          ("C-z C-s r" . synosaurus-choose-and-replace)))

(use-package tex
  ;; Use AUCTeX, since it is better than the built in tex mode.
  ;; Tweak .gitmodules to make the git repository resemble the elpa package.
  ;; Let package latex load tex.
  :preface
  (defcustom TeX-master t
    "The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell AUCTeX the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file.

If this variable is nil, AUCTeX will query you for the name.

If the variable is t, AUCTeX will assume the file is a master file
itself.

If the variable is 'shared, AUCTeX will query for the name, but not
change the file.

If the variable is 'dwim, AUCTeX will try to avoid querying by
attempting to `do what I mean'; and then change the file.

It is suggested that you use the File Variables (see the info node in
the Emacs manual) to set this variable permanently for each file."
    :group 'TeX-command
    :group 'TeX-parse
    :type '(choice (const :tag "Query" nil)
                   (const :tag "This file" t)
                   (const :tag "Shared" shared)
                   (const :tag "Dwim" dwim)
                   (string :format "%v")))
  (make-variable-buffer-local 'TeX-master)
  (put 'TeX-master 'safe-local-variable
       '(lambda (x)
          (or (stringp x)
              (member x (quote (t nil shared dwim))))))
  :custom
  (TeX-auto-local ".auctex-auto-local")
  (TeX-auto-save t)
  (TeX-electric-escape nil)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (TeX-engine 'default)
  (TeX-parse-self t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  (reftex-plug-into-AUCTeX t)
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package text-mode
  :no-require t
  :hook
  (text-mode . turn-on-auto-fill)
  (text-mode . turn-on-flyspell))

(use-package tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package transient
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected)))

(use-package undo-tree
  :commands
  global-undo-tree-mode
  :defer 10
  :config
  (global-undo-tree-mode)
  :delight undo-tree-mode " 🌴")

(use-package unfill
  :bind (("M-q" . unfill-toggle)))

(use-package which-key
  :delight which-key-mode)

(use-package wordnut
  :bind* (("C-z C-w" . wordnut-search)))

(use-package yasnippet
  :commands
  yas-expand-from-trigger-key
  yas-global-mode
  :defer 5
  ;; I fail to use alternative keys in yas-keymap and yas-minor-mode-map as explained in
  ;; https://github.com/capitaomorte/yasnippet/blob/master/doc/faq.org.
  ;; However, everything works fine, sofar.
  :config
  (yas-global-mode 1)
  :delight yas-minor-mode " ✀")

(progn                                  ; startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (fringe-mode '(nil . 0))  ; left-only
              (setq-default indicate-buffer-boundaries 'left)))
  (add-hook 'after-init-hook
            (lambda ()
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
