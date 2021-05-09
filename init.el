;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
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
    (let ((enough 5))
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
  (setq-default                         ; `emacs'
   cursor-type 'box
   indent-tabs-mode nil
   tab-width 8)
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
  (require 'borg)
  (borg-initialize))

(progn                                  ; `use-package'
  (require 'use-package)
  (setq use-package-always-defer t)
  (setq use-package-minimum-reported-time 0.001)
  (setq use-package-verbose t))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest t)
  (auto-compile-update-autoloads t)
  :hook
  ((auto-compile-inhibit-compile)
   . auto-compile-inhibit-compile-detached-git-head))

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

(use-package emacs
  :custom
  (garbage-collection-messages t)
  ;; https://tech.toryanderson.com/2020/08/24/helm-duplicates-history/
  (history-delete-duplicates t)
  (history-length 100)
  ;; (info "(emacs) Auto Scrolling")
  (maximum-scroll-margin 0.25)
  (scroll-conservatively 0)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  ;; (info "(vertico) Configuration")
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt)))
  (enable-recursive-minibuffers t)
  :init
  ;; (info "(vertico) Configuration")
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package server
  :when window-system
  :unless (or noninteractive (daemonp))
  :hook (after-init . server-start))

(progn                                  ; startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;; AUCTeX & friends
(use-package bibtex
  :mode ((rx (seq ".bib" eos)) . bibtex-mode)
  :custom
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
  (LaTeX-electric-left-right-brace t "Conflicts with smartparens-mode.")
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-toc
                        LaTeX-section-section
                        LaTeX-section-label))
  :hook ((LaTeX-mode) . LaTeX-math-mode)
  :commands (LaTeX-narrow-to-environment))

(use-package reftex
  :hook ((LaTeX-mode) . reftex-mode)
  :delight (reftex-mode " 📑"))

(use-package reftex-vars
  :custom
  (reftex-default-bibliography '("~/VCS/research/refs.bib"))
  (reftex-plug-into-AUCTeX t))

(use-package tex
  :functions (TeX-in-comment)
  :custom
  (TeX-auto-local ".auctex-auto-local")
  (TeX-auto-save t)
  (TeX-complete-expert-commands t)
  (TeX-electric-escape nil)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (TeX-engine 'luatex)
  (TeX-master t)
  (TeX-parse-self t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  :hook ((LaTeX-mode) . TeX-PDF-mode)
  :commands (TeX-doc)
  :config
  ;; https://emacs.stackexchange.com/questions/17396/indentation-in-square-brackets
  (defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward "^{}[]\\\\" limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\[)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\])
                          (setq count (- count TeX-brace-indent-level)))
                         ((eq char ?\\)
                          (when (< (point) limit)
                            (forward-char)
                            t))))))
        count))))

(use-package tex-buf
  :after tex-site
  :hook
  ((TeX-after-compilation-finished-functions)
   . TeX-revert-document-buffer))

(use-package tex-site
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  ;; https://gitlab.com/jabranham/emacs
  ;; I use AUCTeX, since it is better than the built in tex mode.
  ;; Tweak the ./configure build-step in .gitmodules to make the git
  ;; repository resemble those in the elpa package.
  ;; Do not require auctex, since auctex.el provides no feature
  ;; 'auctex'.
  :demand t)

;; alphabetical order
(use-package aas
  :commands (aas-set-snippets))

(use-package autorevert
  :hook ((dired-mode) . auto-revert-mode)
  :delight (auto-revert-mode))

(use-package avy
  :disabled
  :custom
  (avy-all-windows t)
  :commands (avy-setup-default)
  :bind* (("C-:" . avy-goto-word-1))
  :init
  (avy-setup-default))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path '("~/VCS/research/papers"))
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org"))

(use-package browse-url
  :unless noninteractive
  :preface
  (defun my-browse-url-mpv (url &optional _)
    (start-process "mpv" nil "mpv" url))

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
  ;; https://karthinks.com/software/lazy-elfeed/
  (browse-url-browser-function
   '((".*github.*" . browse-url-generic)
     (".*gitlab.*" . browse-url-generic)
     (".*google.*" . browse-url-generic)
     (".*openstreetmap.org" . browse-url-generic)
     (".*readthedocs.org" . browser-url-generic)
     (".*reddit.com" . browse-url-generic)
     (".*wikipedia.*" . browse-url-generic)
     ("https:\\/\\/www\\.youtu\\.*be." . my-browse-url-mpv)
     ("https://soundcloud.com" . my-browse-url-mpv)
     ("." . eww-browse-url)))
  (browse-url-generic-program (or (when (eq system-type 'darwin)
                                    "open")
                                  (executable-find "firefox")))
  :commands (browse-url
             browse-url-generic))

(use-package company
  ;; https://github.com/dakra/dmacs/blob/master/init.org#company-auto-completion
  ;; https://github.com/CeleritasCelery/emacs.d/blob/master/emacs.org
  ;; https://tychoish.com/post/better-company/
  :unless noninteractive
  :preface
  ;; Because company disables a new local variable (Emacs-28.1).
  (put 'project-vc-merge-submodules 'safe-local-variable #'null)
  :custom
  (company-backends (quote (company-capf
                            company-keywords
                            company-semantic
                            company-files
                            company-etags
                            company-elisp
                            company-yasnippet)))
  (company-show-numbers t)
  :bind (("C-c ." . company-complete)
         ("C-c C-." . company-complete)
         ("C-c s s" . company-yasnippet)
         (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location)))
  :hook (((LaTeX-mode org-mode) . company-mode)
         ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . company-mode)
         ((sly-mode sly-mrepl-mode) . company-mode))
  :delight (company-mode " 👫"))

(use-package compile
  :delight (compilation-in-progress " 👷"))

(use-package consult
  :custom
  (consult-project-root-function #'my-project-root)
  ;; (info "(consult) Use-package example configuration")
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s g" . consult-git-grep)
         ("M-s f" . consult-find)
         ("M-s k" . consult-keep-lines)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s u" . consult-focus-lines)
         ;; Other bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :commands (consult-preview-mode)
  :init
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package crm
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args 'crm-indicator))

(use-package cython-mode
  :mode ((rx (seq ".py" (any "xdi") eos)) . cython-mode))

(use-package dash
  :commands (-flatten)
  :hook
  ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . dash-fontify-mode))

(use-package deadgrep
  :bind ((:map global-map
               ("M-g d" . deadgrep))
         (:map deadgrep-mode-map
               ("C-c C-w" . deadgrep-edit-mode))))

(use-package diff-hl
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :custom
  (diff-hl-draw-borders nil)
  :hook
  ((dired-mode) . diff-hl-dired-mode)
  ((magit-post-refresh) . diff-hl-magit-post-refresh))

(use-package dired
  ;; https://alexschroeder.ch/wiki/2020-07-16_Emacs_everything
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
  ((LaTeX-mode org-mode prog-mode) . display-line-numbers-mode))

(use-package easy-kill
  ;; https://emacsredux.com/blog/2018/11/09/an-easy-kill/
  ;; https://emacsredux.com/blog/2019/01/10/the-emacs-year-in-review/
  :bind (([remap kill-ring-save] . easy-kill)   ;; M-w
         ([remap mark-sexp] . easy-mark)))      ;; C-M-@

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
  :hook ((python-mode) . electric-operator-mode)
  :delight (electric-operator-mode " 💡"))

(use-package elfeed
  ;; https://github.com/skeeto/elfeed
  :custom
  (elfeed-feeds
   '(("http://emacshorrors.com/feed.atom" v-schneidermann)
     ("http://emacsninja.com/feed.atom" v-schneidermann)
     ("http://pragmaticemacs.com/feed/" b-maugham)
     ("http://sachachua.com/blog/category/emacs/feed" s-chua)
     ("http://www.cachestocaches.com/feed" g-stein)
     ("http://www.howardism.org/index.xml" h-abrams)
     ("https://ambrevar.xyz/atom.xml" p-neirhardt)
     ("https://feeds.feedburner.com/InterceptedWithJeremyScahill" j-scahill)
     ("https://nullprogram.com/feed/" c-wellons)
     ("https://oremacs.com/atom.xml" o-krehel)
     ("https://planet.emacslife.com/atom.xml" planet-emacs)
     ("https://protesilaos.com/codelog.xml" p-stavrou)
     ("https://realpython.com/atom.xml" python)
     ("https://sciencescitoyennes.org/feed/" sciences)
     ("https://updates.orgmode.org/feed/updates" org-updates)
     ("https://www.aclu.org/taxonomy/feed-term/2152/feed" aclu)
     ("https://www.bof.nl/rss/" bof)
     ("https://www.democracynow.org/podcast-video.xml" dn)
     ("https://www.laquadrature.net/fr/rss.xml" lqdn)
     ("https://www.lemonde.fr/blog/huet/feed/" sciences)))
  :bind* (("C-x w" . elfeed)))

(use-package elfeed-search
  :preface
  (defcustom my-elfeed-search-filter-alist
    '(("All" "@48-months-ago")
      ("American Civil Liberties Union" "@12-months-ago +aclu")
      ("Ben Maugham" "@48-months-ago +b-maugham")
      ("Bits of Freedom" "@12-months-ago +bof")
      ("Chris Wellons" "@48-months-ago +c-wellons")
      ("Democracy Now" "@12-months-ago +dn")
      ("Greg Stein" "@48-months-ago +g-stein")
      ("Howard Abrams" "@48-months-ago +h-abrams")
      ("Jeremy Scahill" "@12-months-ago +j-scahill")
      ("La Quadrature du Net" "@12-months-ago +lqdn")
      ("Oleh Krehel" "@48-months-ago +o-krehel")
      ("Org Updates" "@12-months-ago +org-updates")
      ("Pierre Neirhardt" "@48-months-ago +p-neirhardt")
      ("Planet Emacs Life" "@12-months-ago +planet-emacs")
      ("Protesilaos Stravrou"  "@48-months-ago +p-stavrou")
      ("Python" "@12-months-ago +python")
      ("Sacha Chua" "@48-months-ago +s-chua")
      ("Sciences" "@48months-ago +sciences")
      ("Starred" "@12-months-ago +*")
      ("Today" "@1-day-ago")
      ("Unread" "@12-months-ago +unread")
      ("Vasilij Schneiderman" "@48-months-ago +v-schneidermann"))
    "List of prompts with filters for `my-set-elfeed-search-filter'."
    :group 'elfeed
    :type '(alist :key-type 'string :value-type 'string))
  :bind ((:map elfeed-search-mode-map
               ("?" . describe-mode)))
  :commands (elfeed-search-set-filter
             elfeed-search-toggle-all)
  :config
  (bind-key
   "f"
   (defun my-set-elfeed-search-filter()
     (interactive)
     (elfeed-search-set-filter
      (cadr (assoc
             (completing-read "Set search filter: "
                              my-elfeed-search-filter-alist nil t)
             my-elfeed-search-filter-alist))))
   elfeed-search-mode-map))

(use-package elfeed-show
  :bind ((:map elfeed-show-mode-map
               ("?" . describe-mode)))
  :config
  (add-hook 'elfeed-show-hook
            (defun on-elfeed-show-hook ()
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

(use-package embark
  :bind (("C-S-a" . embark-act)))

(use-package embark-consult
  :after (consult embark)
  :hook ((embark-collect) . embark-consult-preview-minor-mode)
  :demand t)

(use-package emms
  ;; Let mpd play most sound, and mpv everything else (ideally video only).
  :custom
  (emms-player-list '(emms-player-mpd emms-player-mpv))
  :commands (emms-player-set)
  :hook ((kill-emacs) . emms-stop))

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
  ;; Move `emms-streams-file' to `etc' instead of default `var'.
  (emms-streams-file (no-littering-expand-etc-file-name "emms/streams.el")))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package epkg
  ;; https://github.com/dakra/dmacs/blob/master/init.org#unsortet-stuff-in-no-packages
  :config
  (when (and (fboundp 'eieio-oref)
             (fboundp 'epkg))
    (defun my-borg-check-drone-urls ()
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
    :init
    (add-hook
     'eshell-mode-hook
     (defun on-eshell-mode-hook ()
       (bind-key
        "C-d"
        (defun my-eshell-quit-or-delete-char (arg)
          (interactive "p")
          (if (and (eolp) (looking-back eshell-prompt-regexp nil))
              (eshell-life-is-too-much)
            (delete-char arg)))
        eshell-mode-map)))))

(use-package ess-custom
  :custom
  (inferior-julia-program (executable-find "julia"))
  :demand t)

(use-package ess-julia
  :mode ("\\.jl\\'" . ess-julia-mode)
  :config
  (setq inferior-julia-args "-i --color=yes"))

(use-package eww
  ;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
  ;; https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode
  ;; https://protesilaos.com/dotemacs
  ;; https://www.reddit.com/r/emacs/comments/54kczj/reddit_client_for_emacs/
  :preface
  (defcustom my-eww-readable-sites
    '("www.cnrtl.fr"
      "www.thefreedictionary.com"
      "www.woorden.org")
    "List of urls to show using `eww-readable'."
    :type '(repeat string)
    :group 'eww)

  (defvar my-eww-visited-history nil)

  (defun my-eww-add-to-history ()
    "Store URL in `my-eww-visited-history'.

To be hooked on `eww-after-render-hook'."
    (let ((url (plist-get eww-data :url)))
      (add-to-history 'my-eww-visited-history url)))

  (defun my-eww-browse-dwim (url &optional arg)
    "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.

If URL does not look like a valid link, run a web query using
`eww-search-prefix'.

When called from an eww buffer, provide the current link as
initial input."
    (interactive
     (list
      (completing-read "Run EWW on: "
                       (append my-eww-visited-history eww-prompt-history)
                       nil nil nil 'eww-prompt-history)
      current-prefix-arg))
    (eww url (if arg 4 nil)))

  (defun my-eww-make-readable ()
    (let ((url (eww-current-url)))
      (when (catch 'found
              (mapc
               (lambda (site)
                 (when (string-match (regexp-quote site) url)
                   (throw 'found site)))
               my-eww-readable-sites)
              nil)
        (eww-readable))))

  (defun my-eww-reddit-browser ()
    (interactive)
    (eww-browse-url (format "https://www.reddit.com/r/%s/.mobile"
                            (completing-read "sub-reddit: "
                                             '("emacs"
                                               "i3wm"
                                               "orgmode")
                                             nil t))))
  (defun my-eww-rename-buffer ()
    (let* ((title (plist-get eww-data :title))
           (name (or (and (eq "" title) (plist-get eww-data :url)) title)))
      (rename-buffer (format "*%s # eww*" name) t)))
  :defines
  eww-link-keymap
  eww-mode-map
  :commands (eww-browse-url
             eww-current-url
             eww-open-file
             eww-readable)
  :init
  (add-hook 'eww-after-render-hook #'my-eww-rename-buffer)
  (advice-add 'eww-back-url :after #'my-eww-rename-buffer)
  (advice-add 'eww-back-url :after #'my-eww-rename-buffer)
  (add-hook 'eww-after-render-hook #'my-eww-make-readable)
  (advice-add 'eww-back-url :after #'my-eww-make-readable)
  (advice-add 'eww-back-url :after #'my-eww-make-readable)
  (add-hook 'eww-after-render-hook #'my-eww-add-to-history)
  (advice-add 'eww-back-url :after #'my-eww-add-to-history)
  (advice-add 'eww-back-url :after #'my-eww-add-to-history))

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "GPG_AGENT_INFO"
                                    "SSH_AGENT_PID"
                                    "SSH_AUTH_SOCK"
                                    "PYENV_ROOT"
                                    "PYENV_VERSION"))
  :commands (exec-path-from-shell-initialize)
  :init
  (exec-path-from-shell-initialize)
  :demand t)

(use-package face-remap
  ;; https://protesilaos.com/dotemacs/
  :commands (buffer-face-mode
             buffer-face-set
             variable-pitch-mode)
  :delight (buffer-face-mode)
  :init
  (defun my-toggle-variable-pitch-mode-unless-prog-mode ()
    "Toggle `variable-pitch-mode' unless in `prog-mode'."
    (interactive)
    (unless (derived-mode-p 'prog-mode)
      (call-interactively #'variable-pitch-mode))))

(use-package faces
  :init
  (cond
   ((eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Hack" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Hack")
    (set-face-attribute 'variable-pitch nil :family "FiraGo"))
   ((eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "Hack" :height 110)
    (set-face-attribute 'fixed-pitch nil :family "Hack")
    (set-face-attribute 'variable-pitch nil :family "FiraGo"))
   (t
    (set-face-attribute 'default nil :family "Hack" :height 110)
    (set-face-attribute 'fixed-pitch nil :family "Hack")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))))

(use-package ffap
  :commands (ffap-file-at-point))

(use-package filenotify
  :commands (file-notify-add-watch))

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
  :custom
  (flycheck-check-syntax-automatically (quote (idle-change newline save)))
  (flycheck-mode-line-prefix "⻜")
  :hook ((emacs-lisp-mode python-mode) . flycheck-mode))

(use-package flymake
  :bind ((:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package flyspell
  ;; Leave `flyspell-mode' off, since I prefer `iedit' over `flyspell'.
  :unless noninteractive
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

(use-package geiser-mode
  :custom
  (geiser-mode-start-repl-p t))

(use-package geiser-chez
  :custom
  (geiser-chez-binary "chez"))

(use-package geiser-impl
  :custom
  (geiser-active-implementations
   (list (car (cl-loop for scheme in '(chez guile mit-scheme racket)
                       when (executable-find (symbol-name scheme))
                       collect scheme))))
  ;; Error loading autoloads: (void-function geiser-impl--add-to-alist)
  :commands (geiser-impl--add-to-alist))

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

(use-package goto-addr
  :preface
  (defun my-toggle-goto-address-dwim-mode ()
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (call-interactively #'goto-address-prog-mode)
      (call-interactively #'goto-address-mode)))
  :bind ((:map goto-address-highlight-keymap
               ("C-c C-o" . goto-address-at-point)))
  :hook
  ((compilation-mode eshell-mode shell-mode) . goto-address-mode)
  ((prog-mode) . goto-address-prog-mode))

(use-package gpastel
  ;; GAV: gpaste-3.36.3 fails, but gpaste-3.36.4-r451 works.
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
  (helm-always-two-windows nil)
  (helm-reuse-last-window-split-state nil)
  (helm-split-window-default-side 'same)
  (helm-split-window-inside-p nil)
  (helm-use-frame-when-dedicated-window nil)
  (helm-use-frame-when-more-than-two-windows nil))

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
  :bind ((:map global-map
               ("C-x x" . helm-mini))))

(use-package helm-command
  :custom
  (helm-M-x-reverse-history nil)
  :commands (helm-M-x))

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
  :commands (helm-find-files)
  :bind ((:map global-map
               ("C-x p" . helm-browse-project)
               ("C-x r p" . helm-projects-history)))
  :delight (helm-ff-cache-mode))

(use-package helm-for-files
  :custom
  (helm-file-cache-fuzzy-match t)
  (helm-recentf-fuzzy-match t))

(use-package helm-grep
  ;; https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/
  :preface
  (defun my-helm-rg (directory &optional with-types)
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

  (defun my-helm-default-directory-search (&optional with-types)
    "Grep for a string in `default-directory' using rg.
WITH-TYPES, if non-nil, ask for file types to search in."
    (interactive "P")
    (my-helm-rg default-directory with-types))

  (defun my-helm-project-search (&optional with-types)
    "Grep for a string in current project using rg.
WITH-TYPES, if non-nil, ask for file types to search in."
    (interactive "P")
    (my-helm-rg (my-project-root) with-types))

  :custom
  (helm-grep-ag-command (concat "rg"
                                " --color=never"
                                " --smart-case"
                                " --no-heading"
                                " --line-number %s %s %s"))
  :bind ((:map global-map
               ("M-g a" . helm-do-grep-ag)))
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
  (add-hook 'helm-mode-hook
            (defun on-helm-mode-hook ()
              (if helm-mode
                  (bind-keys :map global-map
                             ("C-x C-f" . helm-find-files)
                             ("M-s o" . helm-occur)
                             ("M-x" . helm-M-x))
                (bind-keys :map global-map
                           ("C-x C-f" . find-file)
                           ("M-s o" . occur)
                           ("M-x" . execute-extended-command)))))
  :delight (helm-mode " 🎯"))

(use-package helm-net
  :custom
  (helm-net-prefer-curl (if (executable-find "curl") t nil)))

(use-package helm-occur
  :commands (helm-occur))

(use-package helm-ring
  :bind ((:map global-map
               ("M-y" . helm-show-kill-ring))))

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
               ("f" . helpful-function)
               ("k" . helpful-key)
               ("o" . helpful-symbol)
               ("u" . helpful-callable)
               ("v" . helpful-variable)
               ("x" . helpful-command)
               ("y" . helpful-macro)
               ("z" . helpful-at-point))))

(use-package hideshow
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding))
  :hook ((prog-mode) . hs-minor-mode)
  :delight (hs-minor-mode " 🙈"))

(use-package hl-line
  :hook
  ((Info-mode elfeed-show-mode emms-playlist-mode) . hl-line-mode)
  ((help-mode magit-status-mode special-mode) . hl-line-mode))

(use-package hydra
  :disabled
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
     ("fs" #'flyspell-mode
      (if (bound-and-true-p flyspell-mode) "[X]" "[ ]"))
     ("g" #'my-toggle-goto-address-dwim-mode
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
   '(("Dired" (mode . dired-mode))
     ("Doc" (or (mode . Info-mode)
                (mode . help-mode)
                (mode . helpful-mode)))
     ("EMMS" (or (mode . emms-lyrics-mode)
                 (mode . emms-mark-mode)
                 (mode . emms-playlist-mode)))
     ("Elisp" (mode . emacs-lisp-mode))
     ("Eshell" (mode . eshell-mode))
     ("Helm" (mode . helm-major-mode))
     ("Magit" (derived-mode . magit-mode))
     ("Org" (mode . org-mode))
     ("PDF" (mode . pdf-view-mode))
     ("Python" (mode . python-mode))
     ("Setup" (derived-mode . conf-mode))
     ("Shell" (mode . shell-mode))
     ("TeX" (or (derived-mode . tex-mode)
                (mode . bibtex-mode)))
     ("VC" (or (mode . vc-annotate-mode)
               (mode . vc-dir-mode)))))
  (ibuffer-saved-filter-groups
   (list
    (make-ibuffer-saved-filter-group
     "Emacs" "Elisp" "Org" "Doc" "Dired" "PDF" "Python" "Eshell" "TeX"
     "Magit" "VC" "Shell" "Helm" "EMMS" "Setup")
    (make-ibuffer-saved-filter-group
     "Python" "Python" "Org" "Doc" "Dired" "PDF" "Elisp" "Eshell" "TeX"
     "Magit" "VC" "Shell" "Helm" "EMMS" "Setup")
    (make-ibuffer-saved-filter-group
     "Text" "Org" "TeX" "PDF" "Doc" "Dired" "Python" "Elisp" "Eshell"
     "Magit" "VC" "Shell" "Helm" "EMMS" "Setup")))
  :commands (ibuffer-switch-to-saved-filter-groups)
  :config
  (add-hook 'ibuffer-mode-hook
            (defun on-ibuffer-mode-hook-to-saved-filter-groups ()
              (ibuffer-switch-to-saved-filter-groups
               (caar ibuffer-saved-filter-groups)))))

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
  :demand t)

(use-package indent
  ;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
  :custom
  (tab-always-indent 'complete)
  :commands (indent-according-to-mode))

(use-package info
  :config
  (push (expand-file-name "etc/info" user-emacs-directory) Info-directory-list))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup t))

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

(use-package laas
  :hook
  ((LaTeX-mode org-mode) . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    "he3 " "\\\(^{3}\\\)He"
                    "he34" "\\\(^{3}\\\)He-\\\(^{4}\\\)He"
                    "he4 " "\\\(^{4}\\\)He"))

(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c e" . macrostep-expand)))
  :commands (macrostep-mode)
  :config
  (use-package use-package)
  :delight (macrostep-mode " 🦵"))

(use-package magit
  ;; https://stackoverflow.com/questions/4114095/how-to-revert-a-git-repository-to-a-previous-commit
  ;; https://stackoverflow.com/questions/9529078/how-do-i-use-git-reset-hard-head-to-revert-to-a-previous-commit
  :bind ((:map global-map
               ("C-x M-g" . magit-dispatch)))
  :custom
  (magit-define-global-key-bindings nil)
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

(use-package magit-files
  :bind ((:map global-map
               ("C-c M-g" . magit-file-dispatch))))

(use-package magit-git
  ;; (magit)Top > Customizing > Essential Settings > Performance >
  ;; MacOS Performance
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

(use-package man
  :custom
  (Man-width 80))

(use-package marginalia
  :commands (marginalia-mode)
  :init
  (marginalia-mode))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc -f markdown -t html5 -s --self-contained --smart")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package maxima
  :interpreter ("maxima" . maxima-mode)
  :mode ((rx ".mac" eos) . maxima-mode))

(use-package message
  ;; https://emacs.stackexchange.com/a/3653
  ;; (info "(emacs) Sending Mail")
  :custom
  (message-sendmail-envelope-from 'header)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  :init
  (use-package sendmail
    :custom
    (mail-specify-envelope-from t)
    (mail-envelope-from 'header)
    (sendmail-program (executable-find "msmtp"))))

(use-package modus-themes
  :commands (modus-themes-load-themes
             modus-themes-load-vivendi)
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package nov
  :mode ((rx (seq ".epub" eos)) . nov-mode))

(use-package novice
  ;; https://www.emacswiki.org/emacs/DisabledCommands
  :preface
  (defun my-enable-this-command (&rest _args)
    "Called when a disabled command is executed.
Enable it and re-execute it."
    (put this-command 'disabled nil)
    (message "You typed %s.  %s was disabled.  It is enabled."
             (key-description (this-command-keys)) this-command)
    (sit-for 0)
    (call-interactively this-command))
  :init
  (setq disabled-command-function #'my-enable-this-command))

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
  (defun my-org-active-current-time-stamp ()
    "Insert an active org-mode `current-time' timestamp."
    (interactive)
    (org-insert-time-stamp (current-time) 'with-hm))

  (defun my-org-inactive-current-time-stamp ()
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

  :custom
  (org-adapt-indentation nil)
  ;; See jupyter's README: jupyter must be the last item, since it
  ;; depends on other languages.
  (org-babel-load-languages (quote ((calc . t)
                                    (emacs-lisp . t)
                                    (eshell . t)
                                    (gnuplot . t)
                                    (julia . t)
                                    (latex . t)
                                    (lisp . t)
                                    (maxima . t)
                                    (org . t)
                                    (python . t)
                                    (scheme . t)
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
  (org-image-actual-width nil)
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
   (if (eq system-type 'darwin)
       '(ol-bibtex
         ol-eshell
         ol-eww
         ol-info
         org-id
         org-mac-link
         org-protocol)
     '(ol-bibtex
       ol-eshell
       ol-eww
       ol-info
       org-id
       org-protocol)))
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
  :commands (org-insert-time-stamp
             org-link-set-parameters
             org-narrow-to-block
             org-narrow-to-subtree)
  :init
  (add-hook
   'org-mode-hook
   (defun on-org-mode-hook-eval-blocks ()
     "Evaluate all \"org-mode-hook-eval-block\" source blocks."
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
             (org-babel-execute-src-block)))))))

(use-package org-agenda
  :after org
  :custom
  (org-agenda-exporter-settings '((ps-landscape-mode t)
                                  (ps-number-of-columns 2)
                                  (ps-paper-type 'a4)
                                  (ps-print-color-p nil)
                                  (ps-print-header nil)))
  (org-agenda-files '("~/VCS/pim/jobs.org"))
  (org-agenda-span 70))

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
      :prepend t :empty-lines 1)))))

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
            (defun on-org-mime-html-hook-change-element-styles ()
              (org-mime-change-element-style
               "pre"
               "color:#E6E1DC; background-color:#232323; padding:0.5em;")
              (org-mime-change-element-style
               "blockquote"
               "border-left: 2px solid gray; padding-left: 4px;"))))

(use-package org-ref
  :after org
  :demand t)

(use-package org-ref-bibtex
  :bind ((:map org-mode-map
               ("C-c j" . org-ref-bibtex-hydra/body))))

(use-package org-ref-core
  :custom
  (org-ref-bibliography-notes "~/VCS/research/notes/notes.org")
  (org-ref-cite-color "LawnGreen")
  (org-ref-ref-color "OrangeRed")
  (org-ref-label-color "DeepPink")
  (org-ref-default-bibliography '("~/VCS/research/refs.bib"))
  (org-ref-pdf-directory '("~/VCS/research/papers"))
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
  (org-latex-listings 'minted)
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

(use-package project
  :preface
  (defun my-project-root ()
    (when-let (project (project-current))
       (car (project-roots project))))
  :commands (project-roots))

(use-package pulse
  ;; https://karthinks.com/software/batteries-included-with-emacs/
  ;; https://www.reddit.com/r/emacs/comments/jwhr6g/batteries_included_with_emacs/
  :preface
  (defun my-pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  :custom
  (pulse-iterations 16)
  (pulse-delay 0.1)
  :init
  (dolist (command (quote (scroll-up-command
                           scroll-down-command
                           recenter-top-bottom
                           other-window)))
    (advice-add command :after #'my-pulse-line)))

(use-package python
  :custom
  (python-shell-interpreter-args "-E -i")
  :interpreter ("python" . python-mode)
  :mode ((rx (seq ".py" (opt "w") eos)) . python-mode)
  :init
  (when (eq system-type 'darwin)
    (let ((pyenv-root (getenv "PYENV_ROOT"))
          (pyenv-version (getenv "PYENV_VERSION")))
      (when (and pyenv-root pyenv-version)
        (setenv "IPYTHONDIR"
                (expand-file-name
                 (concat "~/.ipython-" pyenv-version)))
        (setenv "JUPYTER_CONFIG_DIR"
                (expand-file-name
                 (concat pyenv-root "/versions/" pyenv-version "/etc/jupyter")))
        (setenv "JUPYTER_DATA_DIR"
                (expand-file-name
                 (concat "~/.local/share/jupyter-" pyenv-version)))
        (setenv "JUPYTER_PATH"
                (expand-file-name
                 (concat pyenv-root "/versions/" pyenv-version "/share/jupyter")))
        (setenv "JUPYTER_RUNTIME_DIR"
                (expand-file-name
                 (concat "~/tmpfs/jupyter-" pyenv-version))))))
  :delight (python-mode "🐍 " :major))

(use-package rainbow-delimiters
  :hook
  ((bibtex-mode prog-mode text-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :custom
  (rainbow-x-colors-major-mode-list '(emacs-lisp-mode
                                      ielm-mode
                                      lisp-interaction-mode
                                      c-mode
                                      c++-mode
                                      java-mode))
  :hook
  ((emacs-lisp-mode ielm-mode) . rainbow-mode)
  ((latex-mode lisp-interaction-mode) . rainbow-mode)
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

(use-package repeat
  :commands (repeat))

(use-package reveal
  ;; (info "(magit) Point ends up inside invisible text when jumping to a file-visiting buffer")
  :hook ((magit-diff-visit-file) . reveal-mode)
  :delight (reveal-mode " ⏿"))

(use-package replace
  ;; https://github.com/minad/consult
  ;; https://masteringemacs.org/article/searching-buffers-occur-mode
  :custom
  (list-matching-lines-default-context-lines 0)
  :hook ((occur) . occur-rename-buffer))

(use-package savehist
  :custom
  (savehist-additional-variables
   (quote
    (helm-browse-project-history
     helm-ff-history
     helm-surfraw-engines-history
     kill-ring
     regexp-search-string
     register-alist
     search-string)))
  :commands (savehist-mode)
  :init
  (savehist-mode +1))

(use-package saveplace
  :commands (save-place-mode)
  :config
  (save-place-mode +1))

(use-package select
  :custom
  ;; (info "(emacs) Clipboard")
  (select-enable-clipboard t))

(use-package shell
  :config
  (setenv "PAGER" "cat"))

(use-package shr
  :custom
  (shr-max-image-proportion 0.8)
  ;; (info "(modus-themes) Note on SHR colors")
  (shr-use-colors nil)
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
  ;; (info "(emacs) Clipboard")
  (save-interprogram-paste-before-kill t)
  :hook (((help-mode text-mode) . visual-line-mode)
         ((LaTeX-mode) . turn-on-auto-fill))
  :commands (column-number-mode
             region-active-p)
  :config
  (column-number-mode)
  (add-hook 'overwrite-mode-hook
            (defun on-overwrite-mode-toggle-background-color ()
              "Toggle background-color when toggling overwrite-mode."
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
  :commands (sly
             sly-connected-p
             sly-eval)
  :init
  (add-hook 'sly-mode-hook
            (defun on-sly-mode-hook ()
              (unless (sly-connected-p)
                (save-excursion (sly))))))

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
  :custom
  (sp-lisp-modes '(cider-repl-mode
                   clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   common-lisp-mode
                   emacs-lisp-mode
                   eshell-mode
                   geiser-repl-mode
                   gerbil-mode
                   inf-clojure-mode
                   inferior-emacs-lisp-mode
                   inferior-lisp-mode
                   inferior-scheme-mode
                   lisp-interaction-mode
                   lisp-mode
                   maxima-mode
                   monroe-mode
                   racket-mode
                   racket-repl-mode
                   scheme-interaction-mode
                   scheme-mode
                   slime-repl-mode
                   sly-mrepl-mode
                   stumpwm-mode))
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
  :hook (;; Disable smartparens-mode:
         ((LaTeX-mode) . turn-off-smartparens-mode)
         ;; Enable normal smartparens-mode:
         ((ielm-mode prog-mode text-mode) . turn-on-smartparens-mode)
         ;; Enable strict smartparens-mode:
         ((geiser-repl-mode) . turn-on-smartparens-strict-mode)
         ((ielm-mode) . turn-on-smartparens-strict-mode)
         ((prog-mode) . turn-on-smartparens-strict-mode)
         ((sly-mrepl-mode) . turn-on-smartparens-strict-mode))
  :commands (show-smartparens-global-mode
             smartparens-mode
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
             assq-delete-all
             derived-mode-p
             eval-after-load
             narrow-to-defun
             narrow-to-region
             remq))

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

(use-package toc-org
  :hook ((org-mode) . toc-org-mode))

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

(use-package uniquify
  ;; https://github.com/yiufung/dot-emacs/blob/master/init.el
  :custom
  (uniquify-buffer-name-style 'forward)
  :defer 2)

(use-package url-cookie
  ;; https://alexschroeder.ch/wiki/2020-07-16_Emacs_everything
  :custom
  (url-cookie-trusted-urls nil)
  (url-cookie-untrusted-urls '(".*")))

(use-package vc-hg
  :custom
  (vc-hg-program (or (executable-find "chg")
                     (executable-find "hg"))))

(use-package vertico
  :commands (vertico-mode)
  :init
  (vertico-mode +1)
  (use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion)))))
    :demand t))

(use-package view
  ;; https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84
  ;; https://karthinks.com/software/batteries-included-with-emacs/
  ;; https://www.reddit.com/r/emacs/comments/fojc1y/using_viewmode_for_modal_navigation/
  ;; https://www.youtube.com/watch?v=kZARKLxTeYQ
  :custom
  (view-read-only t)
  :config
  (add-hook 'view-mode-hook
            (defun on-view-mode-hook-change-cursor-type ()
              (setq cursor-type (if view-mode 'hollow 'box))))
  :delight (view-mode " 👀"))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))

(use-package which-key
  :custom
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  :commands (which-key-mode)
  :init
  (which-key-mode +1)
  :delight (which-key-mode))

(use-package winner
  :bind ((:map winner-mode-map
               ("s-<right>" . winner-redo)
               ("s-<left>" . winner-undo)))
  :custom
  (winner-dont-bind-my-keys t))

(use-package with-editor
  :hook ((eshell-mode shell-mode) . with-editor-export-editor))

(use-package wordnut
  :bind* (("C-z C-w" . wordnut-search)))

(use-package writegood-mode
  :commands (writegood-mode))

(use-package ws-butler
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook ((prog-mode text-mode) . ws-butler-mode)
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
