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
  (menu-bar-mode 0))

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
  :demand t
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
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package no-littering
  :demand t)

(use-package epkg
  :custom
  (epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :unless (or noninteractive (daemonp))
  :when window-system
  :no-require t
  :hook
  (after-init . server-start))

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

(use-package bibtex
  :after tex
  :custom
  (bibtex-user-optional-fields
   '(("abstract")
     ("doi" "Digital Object Identifier")
     ("url" "Universal Ressource Locator"))))

(use-package browse-url
  :preface
  (defun dict-en (word)
    "Look up a word in the dictionary at 'http://thefreedictionary.com'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'http://www.thefreedictionary.com' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://www.thefreedictionary.com/" word)))

  (defun dict-nl (word)
    "Look up a word in the dictionary at 'http://www.woorden.org'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'http://www.woorden.org' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://www.woorden.org/zoek/php?woord=" word)))

  (defun dict-fr (word)
    "Look up a word in the dictionary at 'http://www.cnrtl.fr'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search http://www.cnrtl.fr for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://www.cnrtl.fr/definition/academie9/" word)))

  (defun webster (word)
    "Look up a word in the dictionary at 'http://webster-dictionary.org'."
    (interactive
     (list (if (use-region-p)
               (buffer-substring (region-beginning) (region-end))
             (read-string "Search 'http://webster-dictionary.org' for: "
                          (thing-at-point 'word)))))
    (browse-url (concat "http://webster-dictionary.org/definition/" word)))

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
   '((".*google.*" . browse-url-generic)
     (".*reddit.com" . browse-url-generic)
     (".*youtube.*" . browse-url-generic)
     ("." . eww-browse-url)))
  (browse-url-generic-program (executable-find "firefox"))
  :commands
  browse-url)

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
  ((LaTeX-mode emacs-lisp-mode org-mode) . company-mode)
  :delight " 𝍎")

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (concat
                                    ;; file names beginning with # or .
                                    "\\(?:\\`[#\\.]\\)"
                                    ;; file names ending with # or ~
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"))
  (counsel-grep-swiper-limit (lsh 1 20))
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-command-only)
  :commands
  counsel-linux-app-format-function-command-only
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

(use-package elec-pair
  :preface
  (defun my-enable-electric-pair-mode ()
    (electric-pair-mode 1))
  :commands
  electric-pair-mode
  :hook
  (prog-mode . my-enable-electric-pair-mode))

(use-package elfeed
  :preface
  (defun my-elfeed-show-visit-external ()
    "Wrapper to visit the current entry using an external browser."
    (interactive)
    (let ((browse-url-generic-program (executable-find "firefox")))
      (elfeed-show-visit t)))
  (defun my-elfeed-show-shr-browse-url-external ()
    "Wrapper to browse the URL under point using an external browser."
    (interactive)
    (shr-browse-url t))
  (defun my-elfeed-toggle-star ()
    "Wrapper to toggle all starred elfeed search entries."
    (interactive)
    (elfeed-search-toggle-all '*))
  :custom
  (elfeed-feeds
   '(("http://emacshorrors.com/feed.atom" schneidermann)
     ("http://emacsninja.com/feed.atom" schneidermann)
     ("http://nullprogram.com/feed/" wellons)
     ("http://planet.emacsen.org/atom.xml" emacsen)
     ("http://pragmaticemacs.com/feed/" maugham)
     ("https://act.eff.org/action.atom" eff)
     ("https://feeds.feedburner.com/InterceptedWithJeremyScahill" intercepted)
     ("https://sachachua.com/blog/category/emacs-news/feed" chua)
     ("https://www.aclu.org/taxonomy/feed-term/2152/feed" aclu)
     ("https://www.bof.nl/rss/" bof)
     ("https://www.democracynow.org/podcast-video.xml" dn)
     ("https://www.laquadrature.net/fr/rss.xml" lqdn)))
  (elfeed-enclosure-default-dir (expand-file-name "~/tmpfs/"))
  :bind* ("C-x w" . elfeed)
  :commands
  elfeed-search-set-filter
  elfeed-search-toggle-all
  elfeed-search-update--force
  elfeed-show-visit
  :config
  (bind-keys :map elfeed-show-mode-map
             ("B" . my-elfeed-show-visit-external)
             ("&" . my-elfeed-show-shr-browse-url-external))
  (bind-key
   "j"
   (defhydra hydra-elfeed-filter ()
     "filter"
     ("a" (elfeed-search-set-filter "@6-months-ago +aclu") "aclu")
     ("b" (elfeed-search-set-filter "@6-months-ago +bof") "bof")
     ("c" (elfeed-search-set-filter "@6-months-ago +chua") "chua")
     ("d" (elfeed-search-set-filter "@6-months-ago +dn") "dn")
     ("e" (elfeed-search-set-filter "@6-months-ago +emacsen") "emacsen")
     ("f" (elfeed-search-set-filter "@6-months-ago +eff") "eff")
     ("i" (elfeed-search-set-filter "@6-months-ago +intercepted") "intercepted")
     ("l" (elfeed-search-set-filter "@6-months-ago +lqdn") "lqdn")
     ("m" (elfeed-search-set-filter "@6-months-ago +maugham") "maugham")
     ("s" (elfeed-search-set-filter "@6-months-ago +schneidermann") "schneidermann")
     ("w" (elfeed-search-set-filter "@6-months-ago +wellons") "wellons")
     ("*" (elfeed-search-set-filter "@6-months-ago +*") "*")
     ("A" (elfeed-search-set-filter "@6-months-ago") "All")
     ("S" my-elfeed-toggle-star "Star")
     ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
     ("q" nil "quit" :color blue))
   elfeed-search-mode-map))

(use-package epa
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (when (eq system-type 'darwin)
    (setq epg-gpg-program "gpg2")))

(use-package eww
  :preface
  (defcustom eww-readable-sites
    '("www.cnrtl.fr"
      "www.thefreedictionary.com"
      "www.woorden.org")
    "Customized with use-package eww"
    :type '(repeat string)
    :group 'eww)
  ;; https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode
  (defun my-eww-readable ()
    (let ((url (eww-current-url)))
      (when (catch 'found
              (mapc (lambda (site)
                      (when (string-match (regexp-quote site) url)
                        (throw 'found site)))
                    eww-readable-sites)
              nil)
        (eww-readable))))
  ;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
  (defun my-eww-rename-buffer ()
    (rename-buffer "eww" t))
  ;; https://www.reddit.com/r/emacs/comments/54kczj/reddit_client_for_emacs/.
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
  ((eww-mode . my-eww-rename-buffer)
   (eww-after-render . my-eww-readable))
  :commands
  eww-browse-url
  eww-current-url
  eww-open-file
  eww-readable)

(use-package exec-path-from-shell
  :demand t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_AGENT_INFO"))
  :commands
  exec-path-from-shell-initialize
  :init
  (exec-path-from-shell-initialize))

(use-package exwm
  :preface
  (defun my-exwm-invoke (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun my-exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun my-exwm-manage-finish ()
    (when (or (string= exwm-class-name "XTerm")
              (string= exwm-class-name "kitty"))
      (call-interactively #'exwm-input-release-keyboard)))

  :when (getenv "EXWM")
  :custom
  (display-time-string-forms '((format-time-string "%F %R")))
  (exwm-layout-show-all-buffers t)
  (exwm-workspace-number 2)
  (exwm-workspace-show-all-buffers t)
  ;; Line-editing shortcuts
  (exwm-input-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))
  :hook
  (exwm-update-class . my-exwm-update-class)
  (exwm-manage-finish . my-exwm-manage-finish)
  :commands
  exwm-enable
  exwm-input-release-keyboard
  exwm-input-set-key
  exwm-input-toggle-keyboard
  exwm-reset
  exwm-workspace-rename-buffer
  exwm-workspace-switch
  exwm-workspace-switch-create
  :init
  (exwm-enable)
  :config
  ;; Bind 's-' prefix exwm specific keys when exwm gets configured,
  ;; since those key-bindings may conflict with other window managers.
  (exwm-input-set-key (kbd "s-o") #'ace-window)
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  (dotimes (i 10)
    ;; 's-0', 's-1', ..., 's-9'.
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-i' and "s-&" : Invoke application.
  (exwm-input-set-key (kbd "s-i") #'my-exwm-invoke)
  (exwm-input-set-key (kbd "s-&") #'my-exwm-invoke)
  (fringe-mode 4)
  (display-time-mode 1))

;;; Setup `exwm-randr'.
(use-package exwm-randr
  :preface
  ;; https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
  ;; https://github.com/ch11ng/exwm/wiki
  (defun exwm-auto-toggle-screen ()
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (if (search-forward "VGA1 connected" nil 'noerror)
          (start-process-shell-command
           "xrandr" nil "xrandr --output VGA1 --primary --auto --output LVDS1 --off")
        (start-process-shell-command
         "xrandr" nil "xrandr --output LVDS1 --auto"))))

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

  (defun my-exwm-randr-screen-change ()
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
      (setq exwm-randr-workspace-output-plist wop)
      (start-process-shell-command "xrandr" nil command)))

  :when (and (getenv "EXWM")
             (string= (system-name) "venus"))
  :hook
  (exwm-randr-screen-change . my-exwm-randr-screen-change)
  :commands
  exwm-randr-enable
  :init
  (exwm-randr-enable))

(use-package flycheck
  :commands
  flycheck-mode)

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package goto-addr
  :preface
  (defun my-enable-goto-address-mode ()
    (goto-address-mode 1))
  :commands
  goto-address-mode
  :hook
  (prog-mode . my-enable-goto-address-mode))

(use-package help
  :no-require t
  :commands
  temp-buffer-resize-mode
  :init
  (temp-buffer-resize-mode))

(use-package hl-line
  :no-require t
  :commands
  global-hl-line-mode
  :init
  (global-hl-line-mode))

(use-package hydra
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
^^^^        [_g_] insert-register    [_u_] undo     [_q_] quit
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

     ("q" nil nil)))

  (bind-key*
   "C-z C-t"
   (defhydra hydra-toggle-mode (:hint none)
     "
Toggle mode:
_a_  ?a? auto-fill             _i_ ?i? iimage         _v_  ?v? view
_c_  ?c? column-number         _o_ ?o? org-table      _wg_ ?wg? writegood
_d_  ?d? display-line-numbers  _p_ ?p? electric-pair  _wk_ ?wk? which-key
_fc_ ?fc? flycheck                                   _ws_ ?ws? white-space
_fl_ ?fl? font-lock
_fs_ ?fs? flyspell              _r_ ?r? read-only
_g_  ?g? goto-address          _t_ ?t? indent-tabs    _z_  zap
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
     ("i" #'iimage-mode
      (if (bound-and-true-p iimage-mode) "[X]" "[ ]"))
     ("o" #'orgtbl-mode
      (if (bound-and-true-p orgtbl-mode) "[X]" "[ ]"))
     ("p" #'electric-pair-mode
      (if (bound-and-true-p electric-pair-mode) "[X]" "[ ]"))
     ("r" #'read-only-mode
      (if (bound-and-true-p buffer-read-only) "[X]" "[ ]"))
     ("t" (setq indent-tabs-mode (not (bound-and-true-p indent-tabs-mode)))
      (if (bound-and-true-p indent-tabs-mode) "[X]" "[ ]"))
     ("v" #'view-mode
      (if (bound-and-true-p view-mode) "[X]" "[ ]"))
     ("wg" #'writegood-mode
      (if (bound-and-true-p writegood-mode) "[X]" "[ ]"))
     ("wk" #'which-key-mode
      (if (bound-and-true-p which-key-mode) "[X]" "[ ]"))
     ("ws" #'whitespace-mode
      (if (bound-and-true-p whitespace-mode) "[X]" "[ ]"))
     ("z" (message "Abort") :exit t)))
  :commands
  hydra--call-interactively-remap-maybe
  hydra-default-pre
  hydra-keyboard-quit
  hydra-set-transient-map
  hydra-show-hint)

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

(use-package lisp-mode
  :no-require t
  :preface
  (defun my-indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  (emacs-lisp-mode . reveal-mode)
  (lisp-interaction-mode . my-indent-spaces-mode))

(use-package lispy
  :commands
  lispy-mode
  :hook
  ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . lispy-mode)
  :delight lispy-mode " 🗘")

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

(use-package org
  :preface
  (defun find-broken-org-file-links ()
    "Find broken org-mode file links in an org-mode buffer."
    (interactive)
    (if (eq major-mode 'org-mode)
        (let ((paths
               (org-element-map (org-element-parse-buffer 'object) 'link
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

  (defun my-org-mode-hook-eval-blocks ()
    "Evaluate all org-mode source blocks named `org-mode-hook-eval-block'."
    (interactive)
    (if (eq major-mode 'org-mode)
        (let ((blocks
               (org-element-map (org-element-parse-buffer) 'src-block
                 (lambda (element)
                   (when (string= "org-mode-hook-eval-block"
                                  (org-element-property :name element))
                     element)))))
          (dolist (block blocks)
            (goto-char (org-element-property :begin block))
            (org-babel-execute-src-block)))))

  (defun my-org-mode-hook-completion-at-point ()
    (setq completion-at-point-functions
          '(my-org-ref-completion-at-point-ref t)))

  (defun my-org-ref-completion-at-point-ref ()
    (when (looking-back "\\(\\|C\\|auto\\|c\\|eq\\|name\\|page\\)ref:" 8)
      (let ((label (ivy-read "label: " (org-ref-get-labels) :require-match t)))
        (insert label))))

  :custom
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/tmpfs/tasks.org" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("p" "Protocol" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry (file+headline "~/tmpfs/notes.org" "Inbox")
      "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (org-catch-invisible-edits 'show-and-error)
  (org-export-with-sub-superscripts '{})
  (org-src-fontify-natively t)
  (org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "DEFERRED"))))
  (org-use-sub-superscripts '{})
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . (lambda (path link)
                                        (message "Open %s" link)
                                        (eww-open-file path)))
                   ("\\.pdf\\'" . emacs)))
  (org-agenda-exporter-settings '((ps-landscape-mode t)
                                  (ps-number-of-columns 2)
                                  (ps-paper-type 'a4)
                                  (ps-print-color-p nil)
                                  (ps-print-header nil)))
  (org-agenda-files '("~/VCS/pim/jobs.org"))
  (org-agenda-span 70)
  (org-babel-python-command "python -E")
  (org-confirm-babel-evaluate nil)

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
  (org-latex-hyperref-template nil)
  (org-latex-logfiles-extensions '("blg" "lof" "log" "lot" "out" "toc"))
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b.aux"
     "pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; Requires CUSTOM_ID property to suppress LaTeX section labels.
  (org-latex-prefer-user-labels t)
  :bind
  (("C-c a"   . org-agenda)
   ("C-c c"   . org-capture)
   ("C-c l"   . org-store-link)
   ("C-c C-l" . org-insert-link-global))
  :mode
  ("\\.org\\'" . org-mode)
  :hook
  (org-mode . my-org-mode-hook-eval-blocks)
  (org-mode . my-org-mode-hook-completion-at-point)
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
  (mapc #'(lambda (element) (add-to-list 'org-link-abbrev-alist element))
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

(use-package org-element
  :functions
  org-element-map
  org-element-parse-buffer
  org-element-property)

(use-package org-protocol
  :demand t
  :after org)

(use-package org-ref
  :after org
  :custom
  (bibtex-completion-bibliography '("~/VCS/research/refs.bib"))
  (bibtex-completion-library-path "~/VCS/research/papers")
  (bibtex-completion-notes-path "~/VCS/research/notes/notes.org")
  (org-ref-bibliography-notes "~/VCS/research/notes/notes.org")
  (org-ref-completion-library 'org-ref-ivy-cite)
  (org-ref-default-bibliography '("~/tmpfs/refs.bib" "~/VCS/research/refs.bib"))
  (org-ref-pdf-directory "~/VCS/research/papers"))

(use-package org-ref-core
  :commands
  org-ref-get-labels)

(use-package org-ref-glossary
  :commands
  or-follow-glossary)

(use-package org-ref-utils
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
  :demand t
  :commands
  show-paren-mode
  :config
  (show-paren-mode))

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

(use-package peep-dired
  :after dired
  :custom (peep-dired-cleanup-on-disable t)
  :bind (:map dired-mode-map
              ("M-s p" . peep-dired)))

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
    pyvenv-activate)
  :config
  (use-package elpy
    :demand t
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
    (pyvenv-activate "~2.7")
    (elpy-enable)
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

(use-package tex
  ;; Use AUCTeX, since it is better than the built in tex mode.
  ;; Tweak .gitmodules to make the git repository resemble the elpa package.
  ;; Let package latex load tex.
  :preface
  (defcustom TeX-master t
    "*The master file associated with the current buffer.
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
  (TeX-electric-escape t)
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
  (text-mode . turn-on-flyspell)
  (text-mode . indicate-buffer-boundaries-left))

(use-package tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

(use-package yasnippet
  :preface
  (defun yas-ivy-prompt (prompt choices &optional display-fn)
    (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
  :custom
  (yas-prompt-functions
   '(yas-ivy-prompt yas-completing-prompt))
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
  :delight yas-minor-mode " ✀")

(progn                                  ; startup
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

(progn                                  ; personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
