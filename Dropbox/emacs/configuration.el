(setq user-full-name "Goncalo M. V. Henriques"
user-mail-address "gmv.henriques@gmail.com")

(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(add-to-list 'load-path "~/Dropbox/emacs/lisp")

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(defun gh/apply-solarized-theme ()
    (setq solarized-use-variable-pitch nil)
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-dark t))

(defcustom default-light-color-theme 'solarized-light
"default light theme")

(defcustom default-dark-color-theme 'solarized-dark
"default dark theme")

(defun gh/toggle-dark-light-theme ()
(interactive)

(let ((is-light (find default-light-color-theme custom-enabled-themes)))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme (if is-light default-dark-color-theme default-light-color-theme))
  (if (org-mode) (org-mode-restart))))

(global-set-key (kbd "<f12>") 'gh/toggle-dark-light-theme)

(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
                (gh/apply-solarized-theme)))
(gh/apply-solarized-theme))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(when window-system
  (global-hl-line-mode))

(setq column-number-mode t)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-hierarchical-todo-statistics nil)

(setq org-directory "~/Dropbox/org")
(defun org-file-path (filename)
	"Return the absolute address of an org file, given its relative name."
	(concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
	(concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files '("~/Dropbox/org"))

(defun gh/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'gh/mark-done-and-archive)

(setq org-log-done 'time)

(setq org-capture-templates
      '(("a" "Appointment"
	 entry
	 (file  "~/Dropbox/org/calendar.org" )
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

	("b" "Blog idea"
	 entry
	 (file (org-file-path "blog-ideas.org"))
	 "* %?\n")

	("e" "Email" entry
	 (file+headline org-index-file "Inbox")
	 "* TODO %?\n\n%a\n\n")

	("f" "Finished book"
	 table-line (file "~/documents/notes/books-read.org")
	 "| %^{Title} | %^{Author} | %u |")

	("r" "Reading"
	 checkitem
	 (file (org-file-path "to-read.org")))

	("s" "Subscribe to an RSS feed"
	 plain
	 (file "~/documents/rss/urls")
	 "%^{Feed URL} \"~%^{Feed name}\"")

	("t" "Todo"
	 entry
	 (file+headline org-index-file "Inbox")
	 "* TODO %?\n")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun gh/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'gh/open-index-file)

(setq package-check-signature nil)


(use-package org-gcal
  :ensure t
  :config
(setq org-gcal-client-id "107011808994-g9s382a66p4d3f78ibkccl15sjgh7a9n.apps.googleusercontent.com"
	  org-gcal-client-secret "Gjfci0moPki0d_APpcqEL3WF"
	  org-gcal-file-alist '(("gmv.henriques@gmail.com" .  "~/Dropbox/org/calendar.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

(use-package calfw
    :ensure t
    :config
    (use-package calfw-ical
    :ensure t
    :config
    (use-package calfw-org
    :ensure t
    :config
    (setq cfw:display-calendar-holidays nil)
    (defun mycalendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources
       (list
        (cfw:org-create-source "Green")
;	(cfw:ical-create-source "Gcal" "https://calendar.google.com/calendar/ical/gmv.henriques%40gmail.com/private-549e154258dff1844e9f91f62688c84b/basic.ics" "White")
	(cfw:ical-create-source "Feriados" "https://calendar.google.com/calendar/ical/pt-pt.portuguese%23holiday%40group.v.calendar.google.com/public/basic.ics" "Red")
	)))
  )
  )
  )

(defun gh/visit-emacs-config ()
  (interactive)
  (find-file "~/Dropbox/emacs/configuration.org"))

(global-set-key (kbd "C-c e") 'gh/visit-emacs-config)

(global-set-key (kbd "C-x k") 'gh/kill-current-buffer)

(use-package company
:ensure t
:init
(add-hook 'after-init-hook 'global-company-mode)
)

(use-package saveplace
 :ensure t
 :init
 (save-place-mode 1)
)

(setq-default indent-tabs-mode nil)

(use-package yasnippet
:ensure t
:init
  (setq yas-snippet-dirs '("~/Dropbox/emacs/snippets/text-mode"))
  (yas-global-mode 1))

(setq yas/indent-line nil)

(use-package ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (use-package flx-ido
    :ensure t
    :init
    (flx-ido-mode 1) ; better/faster matching
  )

(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers

  (use-package ido-vertical-mode
    :ensure t
    :init
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  )
)

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTex-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defun gh/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun gh/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'gh/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'gh/split-window-right-and-switch)

(global-set-key
[f3]
(lambda ()
    (interactive)
    (ispell-change-dictionary "pt_PT-preao")))

(global-set-key
[f4]
(lambda ()
    (interactive)
    (ispell-change-dictionary "en")))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(use-package flyspell-popup
:ensure t
:init
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct))

(use-package darkroom
   :ensure t)

(use-package centered-window :ensure t)

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (LaTeX-math-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)))
  :config
  (setq TeX-save-query nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)       ;enable document parsing
  (setq-default TeX-master nil) ;make auctex aware of multi-file documents
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-electric-escape t)

  ;; pdfview and auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
   TeX-source-correlate-start-server t)

  ;; refresh buffer
  (add-hook 'TeX-after-compilation-finished-functions
   #'TeX-revert-document-buffer))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

(evil-set-initial-state 'pdf-view-mode 'emacs)
(add-hook 'pdf-view-mode-hook
  (lambda ()
    (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))

(use-package magic-latex-buffer
 :ensure t)
 (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(use-package elfeed-org
  :ensure t
  :config
  (progn
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Dropbox/org/feeds.org"))))

(use-package elfeed
  :ensure t
  :config)

(add-hook 'before-save-hook 'time-stamp)

(setq
  time-stamp-pattern nil
  time-stamp-active t          ; do enable time-stamps
  time-stamp-line-limit 12     ; check first 10 buffer lines for Time-stamp:
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format

(use-package header2
  :config
  (progn

    (defconst gh/header-sep-line-char ?-
      "Character to be used for creating separator lines in header.")

    (defconst gh/header-border-line-char ?=
      "Character to be used for creating border lines in header.")

    (defconst gh/auto-headers-hooks '(latex-mode-hook
                                      LaTeX-mode-hook)
      "List of hooks of major modes in which headers should be auto-inserted.")

    (defvar gh/header-timestamp-cond (lambda () t)
      "This variable should be set to a function that returns a non-nil
      value only when the time stamp is supposed to be inserted. By default, it's
      a `lambda' return `t', so the time stamp is always inserted.")

    (defun gh/turn-on-auto-headers ()
      "Turn on auto headers only for specific modes."
      (interactive)
      (dolist (hook gh/auto-headers-hooks)
        (add-hook hook #'auto-make-header)))

    (defun gh/turn-off-auto-headers ()
      "Turn off auto headers only for specific modes."
      (interactive)
      (dolist (hook gh/auto-headers-hooks)
        (remove-hook hook #'auto-make-header)))


    (defsubst gh/header-sep-line ()
      "Insert separator line"
      (insert header-prefix-string)
      (insert-char gh/header-sep-line-char (- fill-column (current-column)))
      (insert "\n"))

    (defsubst gh/header-border-line ()
      "Insert separator line"
      (insert header-prefix-string)
      (insert-char gh/header-border-line-char (- fill-column (current-column)))
      (insert "\n"))


    (defsubst gh/header-file-name ()
      "Insert \"File Name\" line, using buffer's file name."
      (insert header-prefix-string "File Name          : "
              (if (buffer-file-name)
                  (file-name-nondirectory (buffer-file-name))
                (buffer-name))
              "\n"))

    (defsubst gh/header-author ()
      "Insert current user's name (`user-full-name') as this file's author."
      (insert header-prefix-string "Author             : "
              (user-full-name)
              "\n"))

    (defsubst gh/header-mail ()
      "Insert current user's name (`user-mail-address') as this file's author."
      (insert header-prefix-string "Author e-mail      : "
              user-mail-address
              "\n"))

    (defsubst gh/header-description ()
      "Insert \"Description\" line."
      (insert header-prefix-string "Description        : \n"))

    (defsubst gh/header-creation-date ()
      "Insert todays date as the time of last modification."
      (insert header-prefix-string "Created            : "
              (header-date-string)
              "\n"))

    (defsubst gh/header-timestamp ()
      "Insert field for time stamp."
      (when (funcall gh/header-timestamp-cond)
      (insert header-prefix-string "Time-stamp: <>\n")))

    (defsubst gh/header-modification-date ()
      "Insert todays date as the time of last modification.
       This is normally overwritten with each file save."
      (insert header-prefix-string "Last-Updated       :"
              "\n"))


    (defsubst gh/header-position-point ()
      "Position the point at a particular point in the file.
Bring the point 2 lines below the current point."
      (forward-line 0)
      (newline 2))


    (setq make-header-hook '(gh/header-border-line
                             header-blank
                             gh/header-file-name
                             gh/header-author
                             gh/header-mail
                             gh/header-creation-date
                             header-blank
                             gh/header-sep-line
                             header-blank
                             gh/header-timestamp
                             header-blank
                             gh/header-sep-line
                             header-blank
                             gh/header-description
                             header-modification-date
                             header-blank
                             gh/header-border-line
                             gh/header-position-point))
    (gh/turn-on-auto-headers)
    ))

(use-package dired-details)
(use-package dired+)

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("pdf" . "evince")
          ("mkv" . "vlc")
          ("mp4" . "vlc")
          ("avi" . "vlc"))))

(setq-default dired-listing-switches "-lhvA")

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

(defun dired-xdg-open ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(define-key dired-mode-map (kbd "C-c C-o") 'dired-xdg-open)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package hydra
  :ensure t)

(use-package engine-mode
 :ensure t
 :config
 (engine/set-keymap-prefix (kbd "C-s"))
 (progn
    (defengine priberam
    "https://www.priberam.pt/dlpo/%s"
    :keybinding "p")

 (engine-mode t)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)

  :config
  (use-package evil-magit
    :ensure t)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(defun gh/toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key [f9] 'gh/toggle-maximize-buffer)
